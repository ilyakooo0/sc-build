{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tasks.Build
  ( Build (..),
    HasDockerInfo (..),
  )
where

import Colog
import Control.Concurrent
import Control.GithubCloner
import Control.HasGithubStatus
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Task
import Data.Aeson hiding (Success)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Data.Conduit.Binary as Conduit
import Data.String
import Data.Submission
import Data.Submission.Query
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import qualified Docker.Client as D
import GHC.Generics
import GitHub
import Server.Schema
import System.Directory
import System.Exit
import System.Process.Typed
import System.Random
import UnliftIO.Async
import UnliftIO.Exception

data Build
  = Build
      { preProcessShell :: String,
        dockerFile :: String,
        owner :: Name Owner,
        repoName :: Name Repo,
        sha :: Name Commit,
        timeoutMinutes :: Int
      }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Task Build "build-repo" where
  type
    TaskMonad Build m =
      (MonadUnliftIO m, StaticPQ m, HasDockerInfo m, GithubCloner m, HasGithubStatus m)

  performTask Build {..} =
    bracket createDir (liftIO . removePathForcibly) $ \dir -> flip catchAny (const $ return Failure) $ do
      cloneRepo fullRepoName sha' dir
      (shellCode, _, shellErr) <- readProcess . setWorkingDir dir $ shell preProcessShell
      case shellCode of
        ExitFailure n -> do
          let err = BS.toString shellErr
          logError . T.pack $
            "preprocessing command for repo " <> show fullRepoName <> " at sha "
              <> show sha
              <> " exited with code "
              <> show n
              <> " and error "
              <> err
          scheduleFailedStatus err owner repoName sha
          updateSubmissionStatus fullRepoName sha' (SubmissionFailed err)
        ExitSuccess -> do
          h <- liftIO D.defaultHttpHandler
          dUrl <- getDockerUrl
          let opts = D.defaultCreateOpts imageName
              runDocker ::
                ( MonadIO m,
                  Exception err,
                  WithLog env Message m,
                  StaticPQ m,
                  MonadUnliftIO m,
                  HasGithubStatus m
                ) =>
                D.DockerT IO (Either err a) ->
                m a
              runDocker = runEither <=< liftIO . D.runDockerT (D.defaultClientOpts {D.baseUrl = dUrl}, h)
              imageName = T.pack dir
          liftIO $ writeFile (dir <> "/Dockerfile") dockerFile
          absDir <- liftIO $ makeAbsolute dir
          let waitDockerForever cid =
                D.waitContainer cid >>= \case
                  Left D.GenericDockerError {} -> waitDockerForever cid
                  a -> return a
          cid <- runDocker . runExceptT $ do
            ExceptT $ D.buildImageFromDockerfile (D.defaultBuildOpts imageName) absDir
            ExceptT $ D.createContainer opts Nothing
          dockerRes <- fmap (either id id)
            . race (liftIO (threadDelay (timeoutMinutes * 60000000)) >> return (ExitFailure 1, "Time out", "Time out"))
            $ flip
              finally
              ( runDocker $
                  D.deleteContainer
                    D.ContainerDeleteOpts {deleteVolumes = True, force = True}
                    cid
              )
              . runDocker
              . runExceptT
            $ do
              ExceptT $ D.startContainer D.defaultStartOpts cid
              buildCode <- ExceptT $ waitDockerForever cid
              buildOut <-
                ExceptT $
                  D.getContainerLogsStream
                    D.LogOpts
                      { stdout = True,
                        stderr = False,
                        since = Nothing,
                        timestamps = False,
                        tail = D.All
                      }
                    cid
                    Conduit.sinkLbs
              buildErr <-
                ExceptT $
                  D.getContainerLogsStream
                    D.LogOpts
                      { stdout = True,
                        stderr = True,
                        since = Nothing,
                        timestamps = False,
                        tail = D.All
                      }
                    cid
                    Conduit.sinkLbs
              return (buildCode, buildOut, buildErr)
          case dockerRes of
            (ExitFailure n, _, buildErr) -> do
              let err = BS.toString buildErr
              logError . T.pack $
                "test command for repo " <> show fullRepoName <> " at sha "
                  <> show sha
                  <> " exited with code "
                  <> show n
                  <> " and error "
                  <> err
              scheduleFailedStatus err owner repoName sha
              updateSubmissionStatus fullRepoName sha' (SubmissionFailed err)
            (ExitSuccess, buildOut, _) ->
              case eitherDecode' (BS.dropWhile (/= '{') buildOut) of
                Left err' -> do
                  let err = err' <> " " <> BS.toString buildOut
                  logError . T.pack $
                    "could not decode test result from repo " <> show fullRepoName
                      <> " at sha "
                      <> show sha
                      <> ": "
                      <> err
                  scheduleFailedStatus err owner repoName sha
                  updateSubmissionStatus fullRepoName sha' (SubmissionFailed (err <> BS.toString buildOut))
                Right testResult@TestResult {..} -> do
                  scheduleTestedStatus testResult owner repoName sha
                  updateSubmissionStatus fullRepoName sha' (SubmissionRun testResult)
      return Success
    where
      fullRepoName = T.unpack $ untagName owner <> "/" <> untagName repoName
      sha' = T.unpack $ untagName sha
      runEither ::
        ( Exception err,
          WithLog env Message m,
          StaticPQ m,
          MonadUnliftIO m,
          HasGithubStatus m
        ) =>
        Either err b ->
        m b
      runEither (Left err) = do
        let err' = show err
        logError $ T.pack err'
        scheduleFailedStatus err' owner repoName sha
        updateSubmissionStatus fullRepoName sha' (SubmissionFailed err')
        throwIO err
      runEither (Right a) = return a

createDir :: MonadIO m => m FilePath
createDir = do
  dir <- ("tmp/" <>) <$> getTmpDirName
  liftIO $ createDirectoryIfMissing True dir
  return dir

cloneRepo :: (MonadIO m, GithubCloner m) => String -> String -> FilePath -> m ()
cloneRepo repoName sha path = do
  InstallationToken token <- getGithubInstallationToken
  runProcess_ . setWorkingDir path . fromString $
    "git clone https://x-access-token:" <> token <> "@github.com/" <> repoName <> ".git ."
  runProcess_ . setWorkingDir path . fromString $ "git checkout " <> sha
  return ()

getTmpDirName :: MonadIO m => m FilePath
getTmpDirName = liftIO $ do
  time :: Integer <- round <$> getPOSIXTime
  i <- randomIO @Int
  return $ show time <> "-" <> show i

class HasDockerInfo m where
  getDockerUrl :: m T.Text

instance Exception D.DockerError

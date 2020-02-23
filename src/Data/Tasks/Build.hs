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
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Task
import Control.Task.Scheduler
import Data.Aeson hiding (Success)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Data.String
import Data.Submission
import Data.Submission.Query
import Data.Tasks.StatusUpdate
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
  type TaskMonad Build m = (MonadUnliftIO m, GithubCloner m, StaticPQ m, HasDockerInfo m)

  performTask Build {..} =
    bracket createDir (liftIO . removePathForcibly) $ \dir -> flip catchAny (const $ return Failure) $ do
      cloneRepo fullRepoName sha' dir
      (shellCode, _, shellErr) <- readProcess . setWorkingDir dir $ shell preProcessShell
      case shellCode of
        ExitFailure n -> do
          let err = BS.unpack shellErr
          logError . T.pack $
            "preprocessing command for repo " <> show fullRepoName <> " at sha "
              <> show sha
              <> " exited with code "
              <> show n
              <> " and error "
              <> err
          scheduleTask erroredTask
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
                  MonadUnliftIO m
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
            . race (liftIO (threadDelay (timeoutMinutes * 60000000)) >> return (ExitFailure 1, "Time out"))
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
                  D.getContainerLogs
                    D.LogOpts
                      { stdout = True,
                        stderr = False,
                        since = Nothing,
                        timestamps = False,
                        tail = D.All
                      }
                    cid
              return (buildCode, buildOut)
          case dockerRes of
            (ExitFailure n, buildOut) -> do
              let err = BS.unpack buildOut
              logError . T.pack $
                "test command for repo " <> show fullRepoName <> " at sha "
                  <> show sha
                  <> " exited with code "
                  <> show n
                  <> " and error "
                  <> err
              scheduleTask erroredTask
              updateSubmissionStatus fullRepoName sha' (SubmissionFailed err)
            (ExitSuccess, buildOut) ->
              case eitherDecode' (BS.dropWhile (/= '{') buildOut) of
                Left err' -> do
                  let err = err' <> " " <> BS.unpack buildOut
                  logError . T.pack $
                    "could not decode test result from repo " <> show fullRepoName
                      <> " at sha "
                      <> show sha
                      <> ": "
                      <> err
                  scheduleTask erroredTask
                  updateSubmissionStatus fullRepoName sha' (SubmissionFailed err)
                Right testResult@TestResult {..} -> do
                  let total = M.size tests
                      passed = M.size . M.filter id $ tests
                      description = T.pack $ show passed <> "/" <> show total
                      status =
                        statusTask
                          NewStatus
                            { newStatusState =
                                if total == passed then StatusSuccess else StatusFailure,
                              newStatusTargetUrl = Nothing,
                              newStatusDescription = Just description,
                              newStatusContext = Nothing
                            }
                  scheduleTask status
                  updateSubmissionStatus fullRepoName sha' (SubmissionRun testResult)
      return Success
    where
      statusTask = StatusUpdate owner repoName sha
      erroredTask =
        statusTask $
          NewStatus
            { newStatusState = StatusError,
              newStatusTargetUrl = Nothing,
              newStatusDescription = Just "Build failed",
              newStatusContext = Nothing
            }
      fullRepoName = T.unpack $ untagName owner <> "/" <> untagName repoName
      sha' = T.unpack $ untagName sha
      runEither ::
        ( Exception err,
          WithLog env Message m,
          StaticPQ m,
          MonadUnliftIO m
        ) =>
        Either err b ->
        m b
      runEither (Left err) = do
        let err' = show err
        logError $ T.pack err'
        scheduleTask erroredTask
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
  GithubAccessToken token <- getGithubAccessToken
  GithubUserName username <- getGithubUserName
  runProcess_ . setWorkingDir path . fromString $
    "git clone https://" <> username <> ":" <> token <> "@github.com/" <> repoName <> ".git ."
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

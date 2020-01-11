{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tasks.Build
  ( Build (..),
  )
where

import Colog
import Control.GithubCloner
import Control.Monad.IO.Class
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
import GHC.Generics
import GitHub
import Server.Schema
import System.Directory
import System.Exit
import System.Process.Typed
import System.Random
import UnliftIO.Exception

data Build
  = Build
      { preProcessShell :: String,
        buildCommand :: String,
        owner :: Name Owner,
        repoName :: Name Repo,
        sha :: Name Commit
      }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance Task Build "build-repo" where

  type TaskMonad Build m = (MonadUnliftIO m, GithubCloner m, StaticPQ m)

  performTask Build {..} =
    bracket createDir (liftIO . removePathForcibly) $ \dir -> do
      let fullRepoName = T.unpack $ untagName owner <> "/" <> untagName repoName
          sha' = T.unpack $ untagName sha
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
          (buildCode, buildOut, buildErr) <- readProcess . setWorkingDir dir . fromString $ buildCommand
          case buildCode of
            ExitFailure n -> do
              let err = BS.unpack buildErr
              logError . T.pack $
                "test command for repo " <> show fullRepoName <> " at sha "
                  <> show sha
                  <> " exited with code "
                  <> show n
                  <> " and error "
                  <> err
              scheduleTask erroredTask
              updateSubmissionStatus fullRepoName sha' (SubmissionFailed err)
            ExitSuccess ->
              case decode buildOut of
                Nothing -> do
                  let err = BS.unpack buildOut
                  logError . T.pack $
                    "could not decode test result from repo " <> show fullRepoName
                      <> " at sha "
                      <> show sha
                      <> ": "
                      <> err
                  scheduleTask erroredTask
                  updateSubmissionStatus fullRepoName sha' (SubmissionFailed err)
                Just testResult@TestResult {..} -> do
                  let total = M.size tests
                      passed = M.size . M.filter id $ tests
                      description = T.pack $ show passed <> "/" <> show total
                      status = statusTask NewStatus
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
      erroredTask = statusTask $ NewStatus
        { newStatusState = StatusError,
          newStatusTargetUrl = Nothing,
          newStatusDescription = Just "Build failed",
          newStatusContext = Nothing
        }

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

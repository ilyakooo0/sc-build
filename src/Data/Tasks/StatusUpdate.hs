{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tasks.StatusUpdate
  ( StatusUpdate (..),
  )
where

import Colog
import Control.AppRequestable
import Control.Task
import Data.Aeson hiding (Success)
import qualified Data.Text as T
import GHC.Generics
import GitHub

data StatusUpdate
  = StatusUpdate
      { owner :: Name Owner,
        repoName :: Name Repo,
        sha :: Name Commit,
        status :: NewStatus
      }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromJSON NewStatus where
  parseJSON = withObject "NewStatus" $ \v ->
    NewStatus
      <$> v .: "state"
      <*> v .:? "target_url"
      <*> v .:? "description"
      <*> v .:? "context"

instance Task StatusUpdate "update-commit-ci-status" where
  type TaskMonad StatusUpdate m = (AppRequestable m)

  performTask StatusUpdate {..} = do
    response <- appRequest $ createStatusR owner repoName sha status
    case response of
      Left err -> logError (T.pack $ show err) >> return Failure
      Right st -> logDebug (T.pack $ show st) >> return Success

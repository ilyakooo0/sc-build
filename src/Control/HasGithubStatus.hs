module Control.HasGithubStatus
  ( HasGithubStatus (..),
  )
where

import Colog
import Data.Submission
import GitHub.Data

class HasGithubStatus m where
  schedulePendingStatus :: WithLog env Message m => Name Owner -> Name Repo -> Name Commit -> m ()
  scheduleTestedStatus :: WithLog env Message m => TestResult -> Name Owner -> Name Repo -> Name Commit -> m ()
  scheduleFailedStatus :: WithLog env Message m => String -> Name Owner -> Name Repo -> Name Commit -> m ()

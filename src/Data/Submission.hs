module Data.Submission
  ( Submission (..),
    SubmissionStatus (..),
    TestResult (..),
    Score (..),
  )
where

import Data.Aeson
import Data.Csv
import Data.Int
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL

data Submission
  = Submission
      { userName :: String,
        repoFullName :: String,
        problem :: String,
        sha :: String,
        status :: Jsonb SubmissionStatus
      }
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)

data SubmissionStatus
  = SubmissionRun TestResult
  | SubmissionFailed String
  | BuildScheduled
  deriving (Generic, FromJSON, ToJSON, Eq, Show)

newtype TestResult
  = TestResult
      { tests :: Map Text Bool
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Score
  = Score
      { userName :: String,
        score :: Int64
      }
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo, ToRecord)

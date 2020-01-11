module Control.Task
  ( Task (..),
    Result (..),
  )
where

import Colog
import Data.Aeson hiding (Result)
import Data.Kind
import GHC.Generics
import GHC.TypeLits

data Result
  = Success
  | Failure
  deriving (Eq, Ord, Show, Generic)

class
  (ToJSON t, FromJSON t, KnownSymbol s, Show t) =>
  Task t s
    | s -> t,
      t -> s where

  type TaskMonad t (m :: * -> *) :: Constraint

  performTask :: (TaskMonad t m, WithLog env Message m) => t -> m Result

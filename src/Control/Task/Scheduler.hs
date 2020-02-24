{-# LANGUAGE TypeApplications #-}

module Control.Task.Scheduler
  ( scheduleTask,
    runTasks,
    makeAllRunnable,
  )
where

import Colog
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Task
import Control.Task.Scheduler.Query
import Data.Aeson hiding (Result (..))
import qualified Data.Aeson as A
import Data.Foldable
import Data.Proxy
import qualified Data.Text as T
import Data.Word
import GHC.TypeLits
import Server.Schema
import Squeal.PostgreSQL

scheduleTask ::
  (StaticPQ m, WithLog env Message m, Task t s, MonadUnliftIO m) =>
  t ->
  m ()
scheduleTask = scheduleTaskQuery

runTasks ::
  forall tt m env.
  (StaticPQ m, WithLog env Message m, RunTasks tt m, MonadUnliftIO m) =>
  Word64 ->
  m Int
runTasks n = do
  tasks <- pickTasksQuery (taskNames @tt @m) n
  for_ tasks $ runTask @tt @m
  return $ length tasks

rescheduleTaskTime ::
  forall t s m env.
  (Task t s, StaticPQ m, WithLog env Message m, MonadUnliftIO m) =>
  t ->
  m ()
rescheduleTaskTime = rescheduleTaskQuery

class RunTasks (ts :: [*]) m where
  runTask :: WithLog env Message m => PickedTask -> m ()

  taskNames :: [String]

instance
  forall t ts tt m.
  (Task t ts, TaskMonad t m, StaticPQ m, RunTasks tt m, MonadUnliftIO m) =>
  RunTasks (t ': tt) m
  where
  runTask (PickedTask s (Jsonb v) initialStartTime) | symbolVal @ts Proxy == s =
    case fromJSON @t v of
      A.Error e ->
        logError . T.pack $
          "Coulndn't decode payload for task " <> s
            <> " with payload "
            <> show v
            <> " with error "
            <> e
      A.Success t -> do
        logInfo . T.pack $ "running task " <> task <> " with " <> show t <> " created at " <> show initialStartTime
        r <- performTask @t @ts @m t
        when (r == Failure) $ logWarning . T.pack $ "Task " <> task <> " failed"
        case r of
          Success -> completeTaskQuery t
          Failure -> rescheduleTaskTime t
    where
      task = symbolVal @ts Proxy
  runTask t = runTask @tt @m t

  taskNames = symbolVal @ts Proxy : taskNames @tt @m

instance RunTasks '[] m where
  runTask (PickedTask s (Jsonb v) _) =
    logError . T.pack $ "Unknown task " <> s <> " with payload " <> show v

  taskNames = []

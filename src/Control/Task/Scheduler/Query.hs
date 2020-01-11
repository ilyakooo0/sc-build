{-# LANGUAGE TypeApplications #-}

module Control.Task.Scheduler.Query
  ( scheduleTaskQuery,
    completeTaskQuery,
    rescheduleTaskQuery,
    pickTasksQuery,
    PickedTask (..),
  )
where

import Colog
import Control.Monad.IO.Unlift
import Control.Squeal
import Control.Task
import Data.Aeson hiding (Result (..))
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeLits
import qualified Generics.SOP as SOP
import Server.Schema
import Squeal.PostgreSQL

scheduleTaskQuery ::
  forall m t s env.
  (StaticPQ m, Task t s, WithLog env Message m, MonadUnliftIO m) =>
  t ->
  m ()
scheduleTaskQuery task = do
  logInfo . T.pack $ "schdeuling task: " <> show task
  let query =
        insertInto
          #tasks
          ( Values_ $
              Set (param @1) `as` #task
                :* Set (param @2) `as` #payload
                :* Default `as` #creation_time
          )
          ( OnConflict
              (OnConstraint #pk_task_payload)
              (DoUpdate (Default `as` #creation_time) [])
          )
          (Returning_ Nil)
  dbWrite (const $ return ()) $
    manipulateParams_
      query
      (symbolVal @s Proxy, Jsonb $ toJSON task)

completeTaskQuery ::
  forall m t s env.
  (StaticPQ m, Task t s, WithLog env Message m, MonadUnliftIO m) =>
  t ->
  m ()
completeTaskQuery payload = do
  let task = symbolVal @s Proxy
  logInfo . T.pack $ "Completing task: " <> task <> " with payload " <> show payload
  let query :: Manipulation_ Schema (String, Jsonb Value) ()
      query =
        deleteFrom_
          #tasks
          (param @1 .== #task .&& param @2 .== #payload)
  dbWrite (const $ return ()) $
    manipulateParams_
      query
      (symbolVal @s Proxy, Jsonb $ toJSON payload)

rescheduleTaskQuery ::
  forall s t m env.
  (StaticPQ m, Task t s, WithLog env Message m, MonadUnliftIO m) =>
  t ->
  m ()
rescheduleTaskQuery payload = do
  let task = symbolVal @s Proxy
  logInfo . T.pack $
    "reschdeuling task: " <> task
      <> " with payload "
      <> show payload
  let query :: Manipulation_ Schema (String, Jsonb Value) ()
      query =
        update_
          #tasks
          (Default `as` #creation_time)
          (param @1 .== #task .&& param @2 .== #payload)
  dbWrite (const $ return ()) $
    manipulateParams_
      query
      (symbolVal @s Proxy, Jsonb $ toJSON payload)

pickTasksQuery ::
  forall m env.
  (StaticPQ m, WithLog env Message m, MonadUnliftIO m) =>
  [String] ->
  Word64 ->
  m [PickedTask]
pickTasksQuery tasks limitCount = do
  logDebug "reading tasks"
  let query :: Query_ Schema () PickedTask
      query =
        select_
          ( #task `as` #task
              :* #payload `as` #payload
              :* #creation_time `as` #creationTime
          )
          ( from (table #tasks)
              & where_ (#task `in_` (literal <$> tasks))
              & orderBy [#creation_time & Asc]
              & limit limitCount
          )
  dbRead (const $ return []) $
    runQuery query >>= getRows

data PickedTask
  = PickedTask
      { task :: String,
        payload :: Jsonb Value,
        creationTime :: UTCTime
      }
  deriving (Eq, Show, Generic, SOP.Generic, SOP.HasDatatypeInfo)
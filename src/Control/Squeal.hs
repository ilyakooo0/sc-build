{-# LANGUAGE TypeApplications #-}

module Control.Squeal
  ( db,
    db',
    dbRead,
    dbRead',
    dbWrite,
    dbWrite',
  )
where

import Colog
import Control.Monad.Except
import Control.Monad.IO.Unlift
import qualified Data.Text as T
import Squeal.PostgreSQL

db ::
  forall schemas m env x.
  (MonadPQ schemas m, WithLog env Message m, MonadUnliftIO m) =>
  Maybe TransactionMode ->
  (SquealException -> m x) ->
  m x ->
  m x
db tr err pqx = (trySqueal . maybe id transactionallyRetry tr) pqx >>= \case
  Left e -> do
    logWarning . T.pack $ "SQL ERROR: " <> show e
    err e
  Right x -> return x

dbRead ::
  (MonadPQ schemas m, WithLog env Message m, MonadUnliftIO m) =>
  (SquealException -> m x) ->
  m x ->
  m x
dbRead = db Nothing

dbRead' ::
  ( MonadPQ schemas m,
    MonadError err m,
    WithLog env Message m,
    MonadUnliftIO m
  ) =>
  err ->
  m x ->
  m x
dbRead' = db' Nothing

dbWrite ::
  (MonadPQ schemas m, WithLog env Message m, MonadUnliftIO m) =>
  (SquealException -> m x) ->
  m x ->
  m x
dbWrite = db $ Just defaultWriteTransactionMode

dbWrite' ::
  ( MonadPQ schemas m,
    MonadError err m,
    WithLog env Message m,
    MonadUnliftIO m
  ) =>
  err ->
  m x ->
  m x
dbWrite' = db' $ Just defaultWriteTransactionMode

defaultWriteTransactionMode :: TransactionMode
defaultWriteTransactionMode = TransactionMode RepeatableRead ReadWrite NotDeferrable

db' ::
  ( MonadPQ schemas m,
    MonadError err m,
    WithLog env Message m,
    MonadUnliftIO m
  ) =>
  Maybe TransactionMode ->
  err ->
  m x ->
  m x
db' tr err = db tr (const $ throwError err)

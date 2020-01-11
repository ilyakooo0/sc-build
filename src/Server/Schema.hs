module Server.Schema
  ( StaticPQ,
    Schema,
    migration,
  )
where

import Server.Schema.V1
import Squeal.PostgreSQL

type StaticPQ m = MonadPQ Schema m

type Schema = SchemaV1

migration :: AlignedList (Migration (Terminally PQ IO)) (Public '[]) Schema
migration =
  pureMigration schemaMigrationV1
    :>> Done

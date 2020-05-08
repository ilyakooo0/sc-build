module Server.Schema
  ( StaticPQ,
    Schema,
    migration,
  )
where

import Server.Schema.V1
import Server.Schema.V2
import Squeal.PostgreSQL

type StaticPQ m = MonadPQ Schema m

type Schema = SchemaV2

migration :: AlignedList (Migration (Terminally PQ IO)) (Public '[]) Schema
migration =
  pureMigration schemaMigrationV1
    :>> pureMigration schemaMigrationV2
    :>> Done

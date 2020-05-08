module Server.Schema.V2
  ( SchemaV2,
    schemaMigrationV2,
  )
where

import Data.Aeson
import Data.Int
import Data.Submission
import Data.Text (Text)
import Data.Time
import Server.Schema.V1
import Squeal.PostgreSQL

type SchemaV2 =
  Public
    '[ "tasks"
         ::: 'Table
               ( '[ "pk_task_payload" :=> 'PrimaryKey '["task", "payload"]
                  ]
                   :=> '[ "task" ::: 'NoDef :=> 'NotNull (PG Text),
                          "payload" ::: 'NoDef :=> 'NotNull (PG (Jsonb Value)),
                          "creation_time" ::: 'Def :=> 'NotNull (PG UTCTime),
                          "started" ::: 'Def :=> 'NotNull (PG Bool),
                          "failures" ::: 'Def :=> 'NotNull (PG Int32)
                        ]
               ),
       "submissions"
         ::: 'Table
               ( '["pk_submission_repo_full_name_sha" :=> 'PrimaryKey '["repo_full_name", "sha"]]
                   :=> '[ "user_name" ::: 'NoDef :=> 'NotNull (PG String),
                          "repo_full_name" ::: 'NoDef :=> 'NotNull (PG String),
                          "problem" ::: 'NoDef :=> 'NotNull (PG String),
                          "sha" ::: 'NoDef :=> 'NotNull (PG String),
                          "status" ::: 'NoDef :=> 'NotNull (PG (Jsonb SubmissionStatus))
                        ]
               )
     ]

schemaMigrationV2 :: Migration Definition SchemaV1 SchemaV2
schemaMigrationV2 =
  Migration
    { name = "v2",
      up = alterTable #tasks (addColumn #failures (default_ 0 $ notNullable int)),
      down = alterTable #tasks (dropColumn #failures)
    }

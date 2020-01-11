module Server.Schema.V1
  ( SchemaV1,
    schemaMigrationV1,
  )
where

import Data.Aeson
import Data.Submission
import Data.Text (Text)
import Data.Time
import Squeal.PostgreSQL

type SchemaV1 =
  Public
    '[ "tasks"
         ::: 'Table
               ( '[ "pk_task_payload" :=> 'PrimaryKey '["task", "payload"]
                  ]
                   :=> '[ "task" ::: 'NoDef :=> 'NotNull (PG Text),
                          "payload" ::: 'NoDef :=> 'NotNull (PG (Jsonb Value)),
                          "creation_time" ::: 'Def :=> 'NotNull (PG UTCTime)
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

schemaMigrationV1 :: Migration Definition (Public '[]) SchemaV1
schemaMigrationV1 = Migration
  { name = "v1",
    up =
      createTable
        #tasks
        ( notNullable text `as` #task
            :* notNullable jsonb `as` #payload
            :* default_ now (notNullable timestampWithTimeZone) `as` #creation_time
        )
        (primaryKey (#task :* #payload) `as` #pk_task_payload)
        >>> createTable
          #submissions
          ( notNullable text `as` #user_name
              :* notNullable text `as` #repo_full_name
              :* notNullable text `as` #problem
              :* notNullable text `as` #sha
              :* notNullable jsonb `as` #status
          )
          (primaryKey (#repo_full_name :* #sha) `as` #pk_submission_repo_full_name_sha)
        >>> createIndexes,
    down = dropTable #tasks >>> dropTable #submissions
  }
  where
    createIndexes :: Definition sch sch
    createIndexes =
      UnsafeDefinition
        "CREATE INDEX ON tasks (creation_time);"

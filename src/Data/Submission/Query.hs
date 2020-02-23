{-# LANGUAGE TypeApplications #-}

module Data.Submission.Query
  ( createSubmission,
    updateSubmissionStatus,
    getSubmission,
    getResults,
  )
where

import Colog
import Control.Monad.IO.Unlift
import Control.Squeal
import Data.Submission
import qualified Data.Text as T
import Server.Schema
import Squeal.PostgreSQL

createSubmission ::
  (StaticPQ m, WithLog env Message m, MonadUnliftIO m) =>
  Submission ->
  m ()
createSubmission s@Submission {..} = do
  logInfo . T.pack $ "Creating submission: " <> show s
  let query =
        insertInto
          #submissions
          ( Values_ $
              Set (param @1) `as` #user_name
                :* Set (param @2) `as` #repo_full_name
                :* Set (param @3) `as` #problem
                :* Set (param @4) `as` #sha
                :* Set (param @5) `as` #status
          )
          ( OnConflict
              (OnConstraint #pk_submission_repo_full_name_sha)
              DoNothing
          )
          (Returning_ Nil)
  dbWrite (const $ return ()) $
    manipulateParams_
      query
      (userName, repoFullName, problem, sha, status)

updateSubmissionStatus ::
  (StaticPQ m, WithLog env Message m, MonadUnliftIO m) =>
  String ->
  String ->
  SubmissionStatus ->
  m ()
updateSubmissionStatus repoFullName sha status = do
  let query :: Manipulation_ Schema (Jsonb SubmissionStatus, String, String) ()
      query =
        update_
          #submissions
          (Set (param @1) `as` #status)
          (#repo_full_name .== param @2 .&& #sha .== param @3)
  dbWrite (const $ return ()) $
    manipulateParams_
      query
      (Jsonb status, repoFullName, sha)

getSubmission ::
  (StaticPQ m, WithLog env Message m, MonadUnliftIO m) =>
  String ->
  String ->
  m (Maybe Submission)
getSubmission repoFullName sha = do
  let query :: Query_ Schema (String, String) Submission
      query =
        select_
          ( #user_name `as` #userName
              :* #repo_full_name `as` #repoFullName
              :* #problem `as` #problem
              :* #sha `as` #sha
              :* #status `as` #status
          )
          ( from (table #submissions)
              & where_
                (#repo_full_name .== param @1 .&& #sha .== param @2)
          )
  dbRead (const $ return Nothing) $
    runQueryParams
      query
      (repoFullName, sha)
      >>= firstRow

getResults ::
  (StaticPQ m, WithLog env Message m, MonadUnliftIO m) =>
  String ->
  m [Score]
getResults task = do
  let query :: Query_ Schema (Only String) Score
      query = UnsafeQuery "select a.user_name as \"userName\", max(a.correct) as score from (select (select count(*) as correct from jsonb_each_text(submissions.status -> 'contents' -> 'tests') where value = 'true'), user_name from submissions where problem = $1) as a group by user_name"
  dbRead (const $ return []) $
    runQueryParams
      query
      (Only task)
      >>= getRows

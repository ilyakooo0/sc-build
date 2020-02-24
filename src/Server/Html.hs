{-# OPTIONS_GHC -Wno-orphans #-}

module Server.Html
  ( getSubmissionR,
    MonadHasBaseUrl (..),
    redirectToSubmission,
    getUrl,
  )
where

import Clay as C
import Colog
import Control.Monad.IO.Unlift
import Data.Foldable
import qualified Data.Map as M
import Data.String
import Data.Submission
import Data.Submission.Query
import qualified Data.Text as T
import Server.Schema
import Squeal.PostgreSQL (Jsonb (..))
import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html4.Strict.Attributes as A
import Text.Blaze.Html5 as H

getSubmissionR ::
  (MonadUnliftIO m, StaticPQ m, WithLog env Message m, MonadHasBaseUrl m) =>
  String ->
  String ->
  String ->
  m Markup
getSubmissionR user repo sha' = do
  restartUrl <- (<> "/restart") <$> getUrl (user <> "/" <> repo) sha'
  let restartBuild = do
        H.form H.! A.action (fromString restartUrl) H.! A.method "post" $
          H.input H.! A.type_ "submit" H.! A.value "Restart test"
  (titleText, inner) <- getSubmission (user <> "/" <> repo) sha' >>= \case
    Nothing ->
      return . ("Not found",) $
        H.h1 "404. not found. go away."
    Just Submission {..} -> return . (repo,) $ do
      H.h1 $ toHtml problem
      H.h2 $ toHtml userName
      case status of
        Jsonb BuildScheduled -> do
          H.h3 "Waiting to build ⌛"
          restartBuild
        Jsonb (SubmissionFailed err) -> do
          H.h3 "Failed to build ☠️"
          restartBuild
          H.code . H.pre $ toHtml err
        Jsonb (SubmissionRun (TestResult tests)) -> do
          let total = M.size tests
              passed = M.size . M.filter Prelude.id $ tests
          H.h3 . toHtml $
            "Tests have run: " <> show passed <> "/" <> show total
              <> if passed == total then " ✅" else " 🟨"
          restartBuild
          H.table $ flip M.foldMapWithKey tests $ \testName testPassed -> H.tr $ do
            (H.td H.! A.align "right") . H.p $ toHtml testName
            H.td $ if testPassed then "✅" else "❌"
  return . docTypeHtml $ do
    H.head $ do
      H.title $ toHtml titleText
      H.style . preEscapedToHtml . render $ do
        C.body ? do
          fontFamily [] [sansSerif]
          C.maxWidth (px 800)
          C.display C.block
          C.marginLeft auto
          C.marginRight auto
          C.paddingLeft (px 16)
          C.paddingRight (px 16)
        (C.h1 <> C.h2 <> C.h3) ? do
          textAlign center
        C.pre ? do
          C.whiteSpace C.preWrap
        C.table ? do
          C.width (pct 100)
        (C.tr <> C.td) ? do
          C.width (pct 50)
        C.td ? do
          C.padding (px 4) (px 4) (px 4) (px 4)
        C.form ? do
          C.display C.block
          C.width (pct 100)
        C.input ? do
          C.display C.block
          C.marginLeft auto
          C.marginRight auto
    H.body inner

class MonadHasBaseUrl m where
  getBaseUrl :: m T.Text

getUrl :: (MonadHasBaseUrl m, Monad m) => String -> String -> m String
getUrl fullName sha = do
  siteBase <- getBaseUrl
  return $ T.unpack siteBase <> "/submission/" <> fullName <> "/" <> sha

redirectToSubmission ::
  (MonadHasBaseUrl m, Monad m) =>
  String ->
  String ->
  String ->
  m Markup
redirectToSubmission user repo sha = do
  rUrl <- getUrl (user <> "/" <> repo) sha
  return $ docTypeHtml $ H.head $
    H.meta
      H.! A.httpEquiv "refresh"
      H.! A.content (fromString $ "0; URL=" <> rUrl)

instance ToMarkup [Score] where
  toMarkup xs = docTypeHtml $ do
    H.style . preEscapedToHtml . render $ do
      C.body ? do
        fontFamily [] [sansSerif]
        C.maxWidth (px 800)
        C.display C.block
        C.marginLeft auto
        C.marginRight auto
        C.paddingLeft (px 16)
        C.paddingRight (px 16)
      (C.h1 <> C.h2 <> C.h3) ? do
        textAlign center
      C.pre ? do
        C.whiteSpace C.preWrap
      (C.tr <> C.td) ? do
        C.width (pct 50)
      C.td ? do
        C.padding (px 4) (px 4) (px 4) (px 4)
        C.border solid (px 1) black
        C.margin (px 0) (px 0) (px 0) (px 0)
      C.table ? do
        C.borderCollapse (other "collapse")
        C.width (pct 100)
    H.body
      $ H.table
      $ for_ xs
      $ \(Score user score) -> H.tr $ do
        (H.td H.! A.align "right") . H.code $ toHtml user
        H.td $ toHtml score

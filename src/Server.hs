{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server
  ( runServer,
  )
where

import Colog
import Control.AppRequestable
import Control.Concurrent
import Control.GithubCloner
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Task.Scheduler
import Data.Aeson
import Data.ByteString
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Submission
import Data.Submission.Query
import Data.Tasks.Build
import Data.Tasks.StatusUpdate
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import GitHub
import GitHub.App.Auth
import GitHub.App.Request
import GitHub.Data.Name
import GitHub.Data.Webhooks.Events
import GitHub.Data.Webhooks.Payload
import Network.Wai.Handler.Warp
import Servant
import Servant.GitHub.Webhook
import Servant.HTML.Blaze
import Server.Config
import Server.Html
import Server.Schema
import Squeal.PostgreSQL
import System.IO
import Text.Blaze.Html
import UnliftIO.Exception (try)

type API =
  "github-webhook"
    :> ( GitHubEvent '[ 'WebhookInstallationEvent]
           :> GitHubSignedReqBody '[JSON] InstallationEvent
           :> Post '[JSON] ()
           :<|> GitHubEvent '[ 'WebhookPushEvent]
             :> GitHubSignedReqBody '[JSON] PushEvent
             :> Post '[JSON] ()
       )
    :<|> "submission"
      :> Capture "user name" String
      :> Capture "repo name" String
      :> Capture "sha" String
      :> Get '[HTML] Markup
    :<|> "submission"
      :> Capture "user name" String
      :> Capture "repo name" String
      :> Capture "sha" String
      :> "restart"
      :> Post '[HTML] Markup

webhookInstallation :: MonadIO m => RepoWebhookEvent -> ((), InstallationEvent) -> m ()
webhookInstallation _ ((), ev) =
  liftIO $ do
    print ev
    hFlush stdout

pendingStatus :: NewStatus
pendingStatus = NewStatus StatusPending Nothing Nothing Nothing

webhookPushEvent ::
  (StaticPQ m, WithLog env Message m, MonadUnliftIO m, HasTasks m) =>
  RepoWebhookEvent ->
  ((), PushEvent) ->
  m ()
webhookPushEvent _ ((), ev) = do
  let repo = evPushRepository ev
      user = T.unpack . whUserLogin . evPushSender $ ev
      fullRepoName = whRepoFullName repo
      [owner, repoName] = T.splitOn "/" fullRepoName
      Just sha = evPushHeadSha ev
  tasks <- getTasks
  let task =
        M.lookupMax $
          M.takeWhileAntitone (repoName >) tasks
  case task of
    Just (name, TaskConfig {..}) | T.take (T.length name) repoName == name -> do
      scheduleTask $ StatusUpdate (N owner) (N repoName) (N sha) pendingStatus
      createSubmission $
        Submission
          user
          (T.unpack fullRepoName)
          (T.unpack name)
          (T.unpack sha)
          (Jsonb BuildScheduled)
      scheduleTask $ Build prebuild build (N owner) (N repoName) (N sha)
      liftIO $ do
        print ev
        hFlush stdout
    _ -> return ()

retestSubmission ::
  (StaticPQ m, WithLog env Message m, MonadUnliftIO m, HasTasks m, MonadHasBaseUrl m) =>
  String ->
  String ->
  String ->
  m Markup
retestSubmission owner repoName sha = do
  let fullRepoName = owner <> "/" <> repoName
      repoName' = T.pack repoName
      sha' = T.pack sha
      owner' = T.pack owner
  getSubmission fullRepoName sha >>= \case
    Nothing -> return ()
    Just _ -> do
      tasks <- getTasks
      let task =
            M.lookupMax $
              M.takeWhileAntitone (repoName' >) tasks
      case task of
        Just (name, TaskConfig {..}) | T.take (T.length name) repoName' == name -> do
          scheduleTask $ StatusUpdate (N owner') (N repoName') (N sha') pendingStatus
          updateSubmissionStatus fullRepoName sha BuildScheduled
          scheduleTask $ Build prebuild build (N owner') (N repoName') (N sha')
        _ -> return ()
  redirectToSubmission owner repoName sha

newtype ServerM (schema :: SchemasType) a
  = ServerM
      { unApp :: ReaderT (ServerData (ServerM schema)) (PQ schema schema IO) a
      }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (ServerData (ServerM schema)),
      MonadPQ schema,
      MonadUnliftIO
    )

server :: ServerT API (ServerM Schema)
server = (webhookInstallation :<|> webhookPushEvent) :<|> getSubmissionR :<|> retestSubmission

runServer :: IO ()
runServer = do
  c@Config {..} <- getConfig
  print c
  auth <- mkInstallationAuth (AppAuth appId appPkPem) installationId
  ts <- newIORef mempty
  pool <- createConnectionPool databseUrl 1 0.5 10
  _ <- forkIO $ do
    updateTasks ts tasksPath
    threadDelay $ 10 * 60 * (10 ^ (6 :: Int))
  let context =
        gitPolyHubKey (return webhookSecret)
          :. EmptyContext
      serverData = ServerData
        { githubAppAuth = auth,
          githubUserName = GithubUserName githubUsername,
          githubAccessToken = GithubAccessToken personalAccessToken,
          tasks = ts,
          logger = simpleMessageAction,
          baseUrl = baseSiteUrl
        }
      repeatIfNotEmpty n f = f >>= \m -> do
        when (m == 0) $ liftIO $ threadDelay n
        repeatIfNotEmpty n f
  _ <-
    forkIO . repeatIfNotEmpty (10 ^ (6 :: Int))
      . usingConnectionPool pool
      . (`runReaderT` serverData)
      . unApp
      $ runTasks @'[Build] 1
  _ <-
    forkIO . repeatIfNotEmpty (10 ^ (6 :: Int))
      . usingConnectionPool pool
      . (`runReaderT` serverData)
      . unApp
      $ runTasks @'[StatusUpdate] 5
  run port $ serveWithContext (Proxy @API) context $
    hoistServerWithContext
      (Proxy @API)
      (Proxy @'[PolyGitHubKey])
      (Handler . ExceptT . try . usingConnectionPool pool . (`runReaderT` serverData) . unApp)
      server

updateTasks :: IORef TasksConfig -> FilePath -> IO ()
updateTasks ref path =
  decodeFileEither path >>= \case
    Right a -> writeIORef ref a
    Left e -> print e

data ServerData m
  = ServerData
      { githubAppAuth :: !InstallationAuth,
        githubUserName :: !GithubUserName,
        githubAccessToken :: !GithubAccessToken,
        tasks :: IORef TasksConfig,
        logger :: LogAction m Message,
        baseUrl :: String
      }

newtype PolyGitHubKey = PolyGitHubKey (forall result. GitHubKey result)

gitPolyHubKey :: IO ByteString -> PolyGitHubKey
gitPolyHubKey k = PolyGitHubKey (Servant.GitHub.Webhook.gitHubKey k)

instance HasContextEntry '[PolyGitHubKey] (GitHubKey result) where
  getContextEntry (PolyGitHubKey x :. _) = x

instance MonadReader (ServerData n) m => GithubCloner m where

  getGithubAccessToken = asks githubAccessToken

  getGithubUserName = asks githubUserName

instance (MonadIO m, MonadReader (ServerData n) m) => AppRequestable m where
  appRequest req = do
    auth <- asks githubAppAuth
    liftIO $ executeAppRequest auth req

instance MonadReader (ServerData n) m => MonadHasBaseUrl m where
  getBaseUrl = asks baseUrl

instance (MonadIO m, MonadReader (ServerData n) m) => HasTasks m where
  getTasks = asks tasks >>= liftIO . readIORef

type TasksConfig = Map T.Text TaskConfig

data TaskConfig
  = TaskConfig
      { prebuild :: String,
        build :: String
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

class HasTasks m where
  getTasks :: m TasksConfig

instance HasLog (ServerData m) Message m where

  getLogAction :: ServerData m -> LogAction m Message
  getLogAction = logger
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> ServerData m -> ServerData m
  setLogAction newLogAction env = env {logger = newLogAction}
  {-# INLINE setLogAction #-}

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
import Control.HasGithubStatus
import Control.Monad.Except
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Task.Scheduler
import Crypto.PubKey.RSA.Read
import Data.Aeson
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSCL
import qualified Data.Csv as Csv
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
import GitHub hiding (Accept)
import GitHub.App.Auth
import GitHub.App.Request
import GitHub.Auth (Auth (OAuth))
import GitHub.Data.Name
import GitHub.Data.Webhooks.Events
import GitHub.Data.Webhooks.Payload hiding (URL (..))
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Media.MediaType
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
import UnliftIO.Exception (catch, displayException, handleAny, throwIO, try)

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
    :<|> "results" :> Capture "task name" String :> Get '[CSV, HTML] [Score]

webhookInstallation :: MonadIO m => RepoWebhookEvent -> ((), InstallationEvent) -> m ()
webhookInstallation _ ((), ev) =
  liftIO $ do
    print ev
    hFlush stdout

webhookPushEvent ::
  (StaticPQ m, MonadReader (ServerData m) m, MonadUnliftIO m, HasTasks m, MonadError ServerError m) =>
  RepoWebhookEvent ->
  ((), PushEvent) ->
  m ()
webhookPushEvent _ ((), ev) = do
  let repo = evPushRepository ev
      user = T.unpack . whUserLogin . evPushSender $ ev
      fullRepoName = whRepoFullName repo
  (owner, repoName) <- case T.splitOn "/" fullRepoName of
    [a, b] -> return (a, b)
    _ -> throwError err422
  sha <- maybe (throwError err422) return $ evPushHeadSha ev
  tasks <- getTasks
  let task =
        M.lookupMax $
          M.takeWhileAntitone (repoName >) tasks
  case task of
    Just (name, TaskConfig {..}) | T.take (T.length name) repoName == name -> do
      schedulePendingStatus (N owner) (N repoName) (N sha)
      createSubmission $
        Submission
          user
          (T.unpack fullRepoName)
          (T.unpack name)
          (T.unpack sha)
          (Jsonb BuildScheduled)
      scheduleTask $ Build prebuild dockerfile (N owner) (N repoName) (N sha) timeoutMinutes
      liftIO $ do
        print ev
        hFlush stdout
    _ -> return ()

retestSubmission ::
  (StaticPQ m, MonadReader (ServerData m) m, MonadUnliftIO m, HasTasks m, MonadHasBaseUrl m) =>
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
          schedulePendingStatus (N owner') (N repoName') (N sha')
          updateSubmissionStatus fullRepoName sha BuildScheduled
          scheduleTask $ Build prebuild dockerfile (N owner') (N repoName') (N sha') timeoutMinutes
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

instance MonadError ServerError (ServerM schema) where
  throwError = throwIO
  catchError = catch

server :: ServerT API (ServerM Schema)
server = (webhookInstallation :<|> webhookPushEvent) :<|> getSubmissionR :<|> retestSubmission :<|> getResults

runServer :: IO ()
runServer = do
  c@Config {..} <- getConfig
  print c
  pem <- BS.readFile appPkPemPath
  appPkPem <- case readRsaPem pem of
    Right k -> return k
    Left e -> error (show e)
  auth <- mkInstallationAuth (AppAuth appId appPkPem) installationId
  ts <- newIORef mempty
  pool <- createConnectionPool databseUrl 1 0.5 10
  _ <- forkIO $ do
    updateTasks ts tasksPath
    threadDelay $ 5 * 60 * (10 ^ (6 :: Int))
  let context =
        gitPolyHubKey (return webhookSecret)
          :. EmptyContext
      serverData =
        ServerData
          { githubAppAuth = auth,
            tasks = ts,
            logger = cfilter ((logSeverity <=) . msgSeverity) richMessageAction,
            baseUrl = baseSiteUrl,
            dockerUrl = T.pack cfgDockerUrl,
            context = githubContext
          }
      repeatIfNotEmpty n f = f >>= \m -> do
        when (m == 0) $ liftIO $ threadDelay n
        repeatIfNotEmpty n f
      printingErrors =
        handleAny (logError . T.pack . displayException >=> const (return 0))
  usingConnectionPool pool
    . (`runReaderT` serverData)
    . unApp
    . handleAny (error . displayException)
    $ makeAllRunnable
  replicateM_ builderCount
    $ forkIO . repeatIfNotEmpty (10 ^ (6 :: Int))
      . usingConnectionPool pool
      . (`runReaderT` serverData)
      . unApp
      . printingErrors
    $ runTasks @'[Build] 1
  _ <-
    forkIO . repeatIfNotEmpty (10 ^ (6 :: Int))
      . usingConnectionPool pool
      . (`runReaderT` serverData)
      . unApp
      . printingErrors
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
        tasks :: IORef TasksConfig,
        logger :: LogAction m Message,
        baseUrl :: T.Text,
        dockerUrl :: T.Text,
        context :: T.Text
      }

newtype PolyGitHubKey = PolyGitHubKey (forall result. GitHubKey result)

gitPolyHubKey :: IO ByteString -> PolyGitHubKey
gitPolyHubKey k = PolyGitHubKey (Servant.GitHub.Webhook.gitHubKey k)

instance HasContextEntry '[PolyGitHubKey] (GitHubKey result) where
  getContextEntry (PolyGitHubKey x :. _) = x

instance (MonadReader (ServerData n) m, MonadIO m, MonadError ServerError m) => GithubCloner m where
  getGithubInstallationToken =
    either
      (\e -> throwError err500 {errBody = BSCL.pack . displayException $ e})
      return
      <=< runExceptT
      $ do
        iAuth <- asks githubAppAuth
        manager <- newTlsManager
        liftIO (obtainAccessToken manager iAuth) >>= either throwError return >>= \case
          OAuth token -> return $ InstallationToken (BSC.unpack token)
          _ -> throwError $ UserError "Not OAuth token. This should never happen."

instance (MonadIO m, MonadReader (ServerData n) m) => AppRequestable m where
  appRequest req = do
    auth <- asks githubAppAuth
    liftIO $ executeAppRequest auth req

instance MonadReader (ServerData n) m => MonadHasBaseUrl m where
  getBaseUrl = asks baseUrl

instance (MonadIO m, MonadReader (ServerData n) m) => HasTasks m where
  getTasks = asks tasks >>= liftIO . readIORef

instance (MonadReader (ServerData m) m, StaticPQ m, MonadUnliftIO m) => HasGithubStatus m where
  schedulePendingStatus nOwner@(N owner) nRepo@(N repo) nCommit@(N sha) = do
    h <- asks baseUrl
    c <- asks context
    scheduleTask $
      StatusUpdate
        nOwner
        nRepo
        nCommit
        ( NewStatus
            StatusPending
            (Just . URL $ h <> "/submission/" <> owner <> "/" <> repo <> "/" <> sha)
            (Just "⌛")
            (Just c)
        )
  scheduleTestedStatus TestResult {..} nOwner@(N owner) nRepo@(N repo) nCommit@(N sha) = do
    h <- asks baseUrl
    c <- asks context
    let total = M.size tests
        passed = M.size . M.filter id $ tests
        description = T.pack $ show passed <> "/" <> show total <> if passed == total then " ✅" else " ❌"
        status =
          StatusUpdate
            nOwner
            nRepo
            nCommit
            NewStatus
              { newStatusState =
                  if total == passed then StatusSuccess else StatusFailure,
                newStatusTargetUrl = Just . URL $ h <> "/submission/" <> owner <> "/" <> repo <> "/" <> sha,
                newStatusDescription = Just description,
                newStatusContext = Just c
              }
    scheduleTask status
  scheduleFailedStatus _ nOwner@(N owner) nRepo@(N repo) nCommit@(N sha) = do
    h <- asks baseUrl
    c <- asks context
    let status =
          StatusUpdate
            nOwner
            nRepo
            nCommit
            NewStatus
              { newStatusState = StatusError,
                newStatusTargetUrl = Just . URL $ h <> "/submission/" <> owner <> "/" <> repo <> "/" <> sha,
                newStatusDescription = Just "Build failed ☠️",
                newStatusContext = Just c
              }
    scheduleTask status

instance (MonadReader (ServerData n) m) => HasDockerInfo m where
  getDockerUrl = asks dockerUrl

type TasksConfig = Map T.Text TaskConfig

data TaskConfig
  = TaskConfig
      { prebuild :: String,
        dockerfile :: String,
        timeoutMinutes :: Int
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

data CSV

instance Accept CSV where
  contentType Proxy = "text" // "csv"

instance Csv.ToRecord a => MimeRender CSV [a] where
  mimeRender Proxy = Csv.encode

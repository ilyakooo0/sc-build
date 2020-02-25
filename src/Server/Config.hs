{-# OPTIONS_GHC -Wno-orphans #-}

module Server.Config
  ( Config (..),
    defaultConfig,
    getConfig,
  )
where

import Colog
import Crypto.PubKey.RSA.Read
import Data.Aeson as A
import Data.ByteString.Char8 as BS
import Data.Text as T
import Data.Yaml
import GHC.Generics
import GitHub.Data.Apps
import GitHub.Data.Id
import GitHub.Data.Installations
import Text.Read (readMaybe)

data Config
  = Config
      { webhookSecret :: !ByteString,
        appId :: !(Id App),
        appPkPemPath :: !FilePath,
        port :: !Int,
        installationId :: !(Id Installation),
        tasksPath :: !FilePath,
        databseUrl :: !ByteString,
        baseSiteUrl :: !Text,
        cfgDockerUrl :: !String,
        logSeverity :: !Severity,
        builderCount :: !Int,
        githubContext :: !Text
      }
  deriving (Show, Generic)

instance FromJSON ByteString where
  parseJSON (String s) = return . BS.pack . T.unpack $ s
  parseJSON _ = fail "not a string"

instance ToJSON ByteString where
  toJSON = String . T.pack . BS.unpack

instance FromJSON Severity where
  parseJSON (String s) = maybe (fail "oh no") return . readMaybe . T.unpack $ s
  parseJSON _ = fail "not a string"

instance ToJSON Severity where
  toJSON = String . T.pack . show

instance FromJSON Config

instance ToJSON Config

defaultConfig :: Config
defaultConfig =
  Config
    { webhookSecret = "GITHUB SECRET",
      appId = Id 69,
      appPkPemPath = "PRIVATE KEY PATH",
      port = 8080,
      installationId = Id 69,
      tasksPath = "TASKS PATH",
      databseUrl = "host=localhost port=5432 dbname=sc-build connect_timeout=10",
      baseSiteUrl = "http://localhost:8080",
      cfgDockerUrl = "http://localhost:1234",
      logSeverity = Info,
      builderCount = 2,
      githubContext = "MY CI NAME"
    }

getConfig :: FilePath -> IO Config
getConfig = decodeFileThrow

-- getConfig :: IO Config
-- getConfig =
--   Config
--     <$> (maybe (error "no GITHUB_WEBHOOK_SECRET") BS8.pack <$> lookupEnv "GITHUB_WEBHOOK_SECRET")
--     <*> (maybe (error "no GITHUB_APP_ID") (Id . read) <$> lookupEnv "GITHUB_APP_ID")
--     <*> readPem
--     <*> (maybe 8080 read <$> lookupEnv "PORT")
--     <*> (maybe (error "no GITHUB_INSTALLATION_ID") (Id . read) <$> lookupEnv "GITHUB_INSTALLATION_ID")
--     <*> (fromMaybe (error "no TASKS") <$> lookupEnv "TASKS")
--     <*> (maybe (error "no DATABASE_URL") BS8.pack <$> lookupEnv "DATABASE_URL")
--     <*> (fromMaybe "http://localhost:8080" <$> lookupEnv "PORT")
--     <*> (fromMaybe  <$> lookupEnv "DOCKER_URL")
--     <*> (fromMaybe "TmpDockerfile" <$> lookupEnv "DOCKER_FILE")
--     <*> (maybe Info read <$> lookupEnv "LOG_SEVERITY")
--   where
--     readPem :: IO PrivateKey
--     readPem = do
--       path <- fromMaybe (error "no app pk") <$> lookupEnv "GITHUB_APP_PK"
--       pem <- BS.readFile path
--       case readRsaPem pem of
--         Right k -> return k
--         Left e -> error (show e)

deriving instance Show ReadRsaKeyError

{-# OPTIONS_GHC -Wno-orphans #-}

module Server.Config
  ( Config (..),
    getConfig,
  )
where

import Crypto.PubKey.RSA (PrivateKey)
import Crypto.PubKey.RSA.Read
import Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe
import GitHub.Data.Apps
import GitHub.Data.Id
import GitHub.Data.Installations
import System.Environment

data Config
  = Config
      { webhookSecret :: !ByteString,
        appId :: !(Id App),
        appPkPem :: !PrivateKey,
        port :: !Int,
        installationId :: !(Id Installation),
        personalAccessToken :: !String,
        githubUsername :: !String,
        tasksPath :: !FilePath,
        databseUrl :: !ByteString,
        baseSiteUrl :: !String
      }
  deriving (Show)

getConfig :: IO Config
getConfig =
  Config
    <$> (maybe (error "no GITHUB_WEBHOOK_SECRET") BS8.pack <$> lookupEnv "GITHUB_WEBHOOK_SECRET")
    <*> (maybe (error "no GITHUB_APP_ID") (Id . read) <$> lookupEnv "GITHUB_APP_ID")
    <*> readPem
    <*> (maybe 8080 read <$> lookupEnv "PORT")
    <*> (maybe (error "no GITHUB_INSTALLATION_ID") (Id . read) <$> lookupEnv "GITHUB_INSTALLATION_ID")
    <*> (fromMaybe (error "no PERSONAL_ACCESS_TOKEN") <$> lookupEnv "PERSONAL_ACCESS_TOKEN")
    <*> (fromMaybe (error "no GITHUB_USERNAME") <$> lookupEnv "GITHUB_USERNAME")
    <*> (fromMaybe (error "no TASKS") <$> lookupEnv "TASKS")
    <*> (maybe (error "no DATABASE_URL") BS8.pack <$> lookupEnv "DATABASE_URL")
    <*> (fromMaybe "http://localhost:8080" <$> lookupEnv "PORT")
  where
    readPem :: IO PrivateKey
    readPem = do
      path <- fromMaybe (error "no app pk") <$> lookupEnv "GITHUB_APP_PK"
      pem <- BS.readFile path
      case readRsaPem pem of
        Right k -> return k
        Left e -> error (show e)

deriving instance Show ReadRsaKeyError

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Data.Yaml (encode)
import Options.Applicative
import Server
import Server.Config

main :: IO ()
main =
  execParser (info (getOptions <**> helper) fullDesc) >>= \case
    PrintDefaultConfig -> putStrLn . BS.unpack . encode $ defaultConfig
    RunServer config -> runServer config

data Run
  = PrintDefaultConfig
  | RunServer FilePath

getOptions :: Parser Run
getOptions =
  subparser
    ( command "default" (info (pure PrintDefaultConfig <**> helper) fullDesc)
    )
    <|> ( RunServer
            <$> strOption
              ( long "config"
                  <> help "Path to yaml config file."
                  <> value "config.yaml"
              )
        )

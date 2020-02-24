module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Data.Yaml
import Server
import Server.Config
import System.Environment

main :: IO ()
main = getArgs >>= \case
  [] -> runServer
  ["default"] -> putStrLn . BS.unpack . encode $ defaultConfig
  xs -> error . show $ xs

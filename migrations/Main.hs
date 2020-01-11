module Main
  ( main,
  )
where

import Data.ByteString.Char8 (pack)
import Server.Schema
import Squeal.PostgreSQL
import System.Environment

main :: IO ()
main = do
  Just dbString <- fmap pack <$> lookupEnv "DATABASE_URL"
  defaultMain dbString migration

module Main where

import qualified Server

import System.Console.ArgParser
  ( ParserSpec
  , Descr(Descr)
  , parsedBy
  , andBy
  , reqPos
  , withParseResult
  )

data StorageServerConfig = StorageServerConfig
  { storagePath :: FilePath
  }

configParser :: ParserSpec StorageServerConfig
configParser = StorageServerConfig
  `parsedBy` reqPos "storage-path" `Descr` "Absolute path to the storage root directory."

main :: IO ()
main =
  withParseResult configParser (\args -> Server.main (storagePath args))

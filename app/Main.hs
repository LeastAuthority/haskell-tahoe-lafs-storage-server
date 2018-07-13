module Main where

import qualified TahoeLAFS.Storage.Server as Server

import Options.Applicative
  ( Parser
  , strOption
  , option
  , auto
  , long
  , metavar
  , value
  , help
  , execParser
  , info
  , progDesc
  , fullDesc
  , helper
  , optional
  , (<**>)
  )

import Data.Semigroup
  ( (<>)
  )

import Network.Wai.Handler.Warp
  ( Port
  )

server :: Parser Server.StorageServerConfig
server = Server.StorageServerConfig
  <$> strOption (long "storage-path"
                 <> metavar "DIRECTORY"
                 <> help "Path to the storage root directory.")
  <*> option auto (long "listen-port"
                 <> metavar "PORT-NUMBER"
                 <> help "TCP port number on which to listen."
                 <> value (8888 :: Port))
  <*> (optional $ strOption (long "certificate-path"
                               <> metavar "FILENAME"
                               <> help "Path to storage server certificate."))
  <*> (optional $ strOption (long "key-path"
                               <> metavar "FILENAME"
                               <> help "Path to storage server key."))

main :: IO ()
main = Server.main =<< execParser opts
  where
    opts = info (server <**> helper)
      ( fullDesc
      <> progDesc "Run a Tahoe-LAFS (GBS) storage server.")

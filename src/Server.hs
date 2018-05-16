module Server
  ( storageAPI
  , storageApp
  , main
  ) where

import Storage
  ( Version(..)
  , ApplicationVersion(..)
  , Version1Parameters(..)
  , StorageAPI
  , api
  )

import Servant
  ( Server
  , serve
  )

import Network.Wai
  ( Application
  )
import Network.Wai.Handler.Warp
  ( run
  )

versionInfo :: Version
versionInfo = Version
  { applicationVersion = "0.1.0"
  , parameters =
    Version1Parameters
    { maximumImmutableShareSize = 2 ^ 16
    , maximumMutableShareSize = 2 ^ 16
    , availableSpace = 2 ^ 32
    , toleratesImmutableReadOverrun = True
    , deleteMutableSharesWithZeroLengthWritev = True
    , fillsHolesWithZeroBytes = True
    , preventsReadPastEndOfShareData = True
    , httpProtocolAvailable = True
    }
  }


storageAPI :: Server StorageAPI
storageAPI = return versionInfo

storageApp :: Application
storageApp = serve api storageAPI

main :: IO ()
main = run 8081 storageApp

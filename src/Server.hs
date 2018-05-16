module Server
  ( storageServer
  , storageApp
  , main
  ) where

import Storage
  ( Version(..)
  , StorageIndex
  , BucketIdentifier
  , StorageBuckets
  , ShareData
  , ShareDataBS
  , ApplicationVersion(..)
  , Version1Parameters(..)
  , AllocateBuckets
  , AllocationResult(..)
  , ReadTestWriteVectors
  , ReadTestWriteResult(..)
  , ReadVectors
  , ReadResult
  , CorruptionDetails
  , StorageAPI
  , api
  )

import Servant
  ( (:<|>)(..)
  , Server
  , Handler(Handler)
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

version :: Handler Version
version = return versionInfo

allocateBuckets :: StorageIndex -> AllocateBuckets -> Handler AllocationResult
allocateBuckets storage_index params =
  return AllocationResult
  { alreadyHave = []
  , allocated = mempty
  }

getBuckets :: StorageIndex -> Handler StorageBuckets
getBuckets storage_index = return mempty

writeToBucket :: BucketIdentifier -> ShareDataBS -> Handler ()
writeToBucket bucket_id share_data = return ()

readFromBucket :: BucketIdentifier -> Handler ShareDataBS
readFromBucket bucket_id = return mempty

adviseCorruptBucket :: BucketIdentifier -> CorruptionDetails -> Handler ()
adviseCorruptBucket bucket_id details = return ()

testvAndReadvAndWritev :: StorageIndex -> ReadTestWriteVectors -> Handler ReadTestWriteResult
testvAndReadvAndWritev storage_index vectors =
  return ReadTestWriteResult
  { success = False
  , readData = mempty
  }

readv :: StorageIndex -> ReadVectors -> Handler ReadResult
readv storage_index vectors = return mempty

storageServer :: Server StorageAPI
storageServer = version
  :<|> allocateBuckets
  :<|> getBuckets
  :<|> writeToBucket
  :<|> readFromBucket
  :<|> adviseCorruptBucket
  :<|> testvAndReadvAndWritev
  :<|> readv

storageApp :: Application
storageApp = serve api storageServer

main :: IO ()
main = run 8081 storageApp

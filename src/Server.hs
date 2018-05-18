module Server
  ( storageServer
  , storageApp
  , main
  ) where

import Storage
  ( Version(..)
  , Size
  , Offset
  , StorageIndex
  , ShareNumber
  , ShareData
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

import Network.HTTP.Types
  ( ByteRange
  , ByteRanges
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

createImmutableStorageIndex :: StorageIndex -> AllocateBuckets -> Handler AllocationResult
createImmutableStorageIndex storage_index params =
  return AllocationResult
  { alreadyHave = mempty
  , allocated = mempty
  }

writeImmutableShare :: StorageIndex -> ShareNumber -> ShareData -> Maybe ByteRanges -> Handler ()
writeImmutableShare storage_index share_number share_data content_ranges = return mempty

adviseCorruptImmutableShare :: StorageIndex -> ShareNumber -> CorruptionDetails -> Handler ()
adviseCorruptImmutableShare storage_index share_number details = return ()

getImmutableShareNumbers :: StorageIndex -> Handler [ShareNumber]
getImmutableShareNumbers storage_index = return mempty

readImmutableShares :: StorageIndex -> [ShareNumber] -> Maybe ByteRanges -> Handler ReadResult
readImmutableShares storage_index share_numbers content_ranges = return mempty

createMutableStorageIndex :: StorageIndex -> AllocateBuckets -> Handler AllocationResult
createMutableStorageIndex = createImmutableStorageIndex

readvAndTestvAndWritev :: StorageIndex -> ReadTestWriteVectors -> Handler ReadTestWriteResult
readvAndTestvAndWritev storage_index vectors =
  return ReadTestWriteResult
  { success = False
  , readData = mempty
  }

readMutableShares :: StorageIndex -> [ShareNumber] -> [Offset] -> [Size] -> ReadVectors -> Handler ReadResult
readMutableShares storage_index share_numbers offsets sizes read_vectors = return mempty

getMutableShareNumbers :: StorageIndex -> Handler [ShareNumber]
getMutableShareNumbers storage_index = return mempty

adviseCorruptMutableShare :: StorageIndex -> ShareNumber -> CorruptionDetails -> Handler ()
adviseCorruptMutableShare storage_index share_number details = return ()

storageServer :: Server StorageAPI
storageServer = version
  :<|> createImmutableStorageIndex
  :<|> writeImmutableShare
  :<|> adviseCorruptImmutableShare
  :<|> getImmutableShareNumbers
  :<|> readImmutableShares
  :<|> createMutableStorageIndex
  :<|> readvAndTestvAndWritev
  :<|> readMutableShares
  :<|> getMutableShareNumbers
  :<|> adviseCorruptMutableShare


storageApp :: Application
storageApp = serve api storageServer

main :: IO ()
main = run 8081 storageApp

module Server
  ( main
  , versionString
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )

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

import System.Posix.StatVFS
  ( StatVFS(statVFS_bsize, statVFS_bavail)
  , statVFS
  )

import System.IO
  ( FilePath
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

versionString = "tahoe-lafhs 0.1.0"

-- Copied from the Python implementation.  Kind of arbitrary.
maxMutableShareSize = 69105 * 1000 * 1000 * 1000 * 1000

version :: FilePath -> Handler Version
version storage = do
  vfs <- liftIO $ statVFS storage
  let available = (toInteger $ statVFS_bsize vfs) * (toInteger $ statVFS_bavail vfs)
  return Version
    { applicationVersion = versionString
    , parameters =
        Version1Parameters
        { maximumImmutableShareSize = available
        , maximumMutableShareSize = maxMutableShareSize
        -- TODO: Copy the "reserved space" feature of the Python
        -- implementation.
        , availableSpace = available
        , toleratesImmutableReadOverrun = True
        , deleteMutableSharesWithZeroLengthWritev = True
        , fillsHolesWithZeroBytes = True
        , preventsReadPastEndOfShareData = True
        , httpProtocolAvailable = True
        }
    }

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

readMutableShares :: StorageIndex -> [ShareNumber] -> [Offset] -> [Size] -> Handler ReadResult
readMutableShares storage_index share_numbers offsets sizes = return mempty

getMutableShareNumbers :: StorageIndex -> Handler [ShareNumber]
getMutableShareNumbers storage_index = return mempty

adviseCorruptMutableShare :: StorageIndex -> ShareNumber -> CorruptionDetails -> Handler ()
adviseCorruptMutableShare storage_index share_number details = return ()

main :: FilePath -> IO ()
main storage = do
  run 8081 (serve api storageServer)
  where
    storageServer :: Server StorageAPI
    storageServer = version storage
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

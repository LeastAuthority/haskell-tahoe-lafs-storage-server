module MemoryBackend
  ( MemoryBackend(MemoryBackend)
  , memoryBackend
  ) where

import Prelude hiding
  ( map
  , lookup
  )

import Data.IORef
 ( IORef
 )

import Data.Map.Strict
  ( Map
  , map
  , keys
  , lookup
  )

import System.Posix.StatVFS
  ( StatVFS(statVFS_bsize, statVFS_bavail)
  , statVFS
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
  , AllocationResult(..)
  , shareNumbers
  )

import Backend
  ( Backend(..)
  )

type MemoryStorage = Map StorageIndex (Map ShareNumber ShareData)

data MemoryBackend = MemoryBackend
  { shares :: MemoryStorage -- Completely written immutable shares
  , buckets :: MemoryStorage -- In-progress immutable share uploads
  } deriving (Show)

instance Backend MemoryBackend where
  version backend =
    return Version
      { applicationVersion = "(memory)"
      , parameters =
          Version1Parameters
          { maximumImmutableShareSize = 1024 * 1024 * 64
          , maximumMutableShareSize = 1024 * 1024 * 64
          , availableSpace = (1024 * 1024 * 1024) - (totalShareSize backend)
          , toleratesImmutableReadOverrun = True
          , deleteMutableSharesWithZeroLengthWritev = True
          , fillsHolesWithZeroBytes = True
          , preventsReadPastEndOfShareData = True
          -- TODO Doesn't really belong here.  Also we need more than a bool.
          -- We need to tell them *where* it is available or it is useless.
          , httpProtocolAvailable = True
          }
      }

  createImmutableStorageIndex backend idx params =
    return AllocationResult
    { alreadyHave = mempty
    , allocated = (shareNumbers params)
    }

  writeImmutableShare backend storage_index share_number share_data content_ranges =
    return mempty

  adviseCorruptImmutableShare backend _ _ _ =
    return mempty

  getImmutableShareNumbers backend storage_index =
    return share_numbers
    where
      share_numbers =
        case lookup storage_index $ shares backend of
          Nothing -> []
          Just shares -> keys shares

  readImmutableShares backend storage_index shares offsets sizes =
    return mempty



totalShareSize :: MemoryBackend -> Size
totalShareSize backend = toInteger $ sum $ map length (shares backend)

memoryBackend :: MemoryBackend
memoryBackend = MemoryBackend mempty mempty

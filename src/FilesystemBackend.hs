module FilesystemBackend
  ( FilesystemBackend(FilesystemBackend)
  ) where

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
  )

import Backend
  ( Backend(..)
  )

data FilesystemBackend = FilesystemBackend FilePath
  deriving (Show)

versionString = "tahoe-lafs (gbs) 0.1.0"

-- Copied from the Python implementation.  Kind of arbitrary.
maxMutableShareSize = 69105 * 1000 * 1000 * 1000 * 1000

instance Backend FilesystemBackend where
  version (FilesystemBackend path) = do
    vfs <- statVFS path
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
          -- TODO Doesn't really belong here.  Also we need more than a bool.
          -- We need to tell them *where* it is available or it is useless.
          , httpProtocolAvailable = True
          }
      }

  createImmutableStorageIndex (FilesystemBackend path) idx params =
    return AllocationResult
    { alreadyHave = mempty
    , allocated = mempty
    }

  writeImmutableShare (FilesystemBackend path) storage_index share_number share_data content_ranges =
    return mempty

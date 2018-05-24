module NullBackend
  ( NullBackend(NullBackend)
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

data NullBackend = NullBackend
  deriving (Show)

instance Backend NullBackend where
  version NullBackend =
    return Version
      { applicationVersion = "(null)"
      , parameters =
          Version1Parameters
          { maximumImmutableShareSize = 0
          , maximumMutableShareSize = 0
          , availableSpace = 0
          , toleratesImmutableReadOverrun = False
          , deleteMutableSharesWithZeroLengthWritev = False
          , fillsHolesWithZeroBytes = False
          , preventsReadPastEndOfShareData = False
          -- TODO Doesn't really belong here.  Also we need more than a bool.
          -- We need to tell them *where* it is available or it is useless.
          , httpProtocolAvailable = True
          }
      }

  createImmutableStorageIndex NullBackend _ _ =
    return AllocationResult
    { alreadyHave = mempty
    , allocated = mempty
    }

  writeImmutableShare NullBackend _ _ _ _ =
    return mempty
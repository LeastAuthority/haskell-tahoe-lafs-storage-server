module TahoeLAFS.Storage.Backend.Null (
    NullBackend (NullBackend),
) where

import TahoeLAFS.Storage.API (
    AllocateBuckets,
    AllocationResult (..),
    ApplicationVersion,
    CorruptionDetails,
    LeaseSecret,
    Offset,
    ReadResult,
    ShareData,
    ShareNumber,
    Size,
    StorageIndex,
    Version (..),
    Version1Parameters (..),
 )

import TahoeLAFS.Storage.Backend (
    Backend (..),
 )

data NullBackend = NullBackend
    deriving (Show)

instance Backend NullBackend where
    version NullBackend =
        return
            Version
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
                        , -- TODO Doesn't really belong here.  Also we need more than a bool.
                          -- We need to tell them *where* it is available or it is useless.
                          httpProtocolAvailable = True
                        }
                }

    createImmutableStorageIndex :: NullBackend -> StorageIndex -> AllocateBuckets -> IO AllocationResult
    createImmutableStorageIndex NullBackend _ _ =
        return
            AllocationResult
                { alreadyHave = mempty
                , allocated = mempty
                }

    writeImmutableShare NullBackend _ _ _ _ =
        return mempty

    adviseCorruptImmutableShare :: NullBackend -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
    adviseCorruptImmutableShare NullBackend _ _ _ =
        return mempty

    getImmutableShareNumbers :: NullBackend -> StorageIndex -> IO [ShareNumber]
    getImmutableShareNumbers NullBackend _ =
        return []

    readImmutableShares :: NullBackend -> StorageIndex -> [ShareNumber] -> [Offset] -> [Size] -> IO ReadResult
    readImmutableShares NullBackend _ _ _ _ =
        return mempty

    renewLease :: NullBackend -> StorageIndex -> [LeaseSecret] -> IO ()
    renewLease NullBackend _ _ = pure mempty

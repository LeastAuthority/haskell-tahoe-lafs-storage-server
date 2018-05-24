
module Backend
  ( Backend(..)
  ) where

import Network.HTTP.Types
  ( ByteRanges
  )

import Storage
  ( Version
  , StorageIndex
  , AllocateBuckets
  , AllocationResult
  , ShareNumber
  , Size
  , Offset
  , ReadResult
  , ReadTestWriteVectors
  , ReadTestWriteResult
  , CorruptionDetails
  , ShareData

  )

class Backend b where
  version :: b -> IO Version

  createImmutableStorageIndex :: b -> StorageIndex -> AllocateBuckets -> IO AllocationResult
  writeImmutableShare :: b -> StorageIndex -> ShareNumber -> ShareData -> Maybe ByteRanges -> IO ()
  adviseCorruptImmutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
  getImmutableShareNumbers :: b -> StorageIndex -> IO [ShareNumber]
  readImmutableShares :: b -> StorageIndex -> [ShareNumber] -> [Offset] -> [Size] -> IO ReadResult

  createMutableStorageIndex :: b -> StorageIndex -> AllocateBuckets -> IO AllocationResult
  readvAndTestvAndWritev :: b -> StorageIndex -> ReadTestWriteVectors -> IO ReadTestWriteResult
  readMutableShares :: b -> StorageIndex -> [ShareNumber] -> [Offset] -> [Size] -> IO ReadResult
  getMutableShareNumbers :: b -> StorageIndex -> IO [ShareNumber]
  adviseCorruptMutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()

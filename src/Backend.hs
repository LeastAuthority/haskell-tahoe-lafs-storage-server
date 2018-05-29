
module Backend
  ( Backend(..)
  , writeMutableShare
  ) where

import Control.Exception
  ( Exception
  , throw
  )

import Data.Map.Strict
  ( fromList
  )

import Network.HTTP.Types
  ( ByteRanges
  )

import Storage
  ( Version
  , StorageIndex
  , AllocateBuckets
  , AllocationResult
  , SlotSecrets(SlotSecrets)
  , ShareNumber
  , Size
  , Offset
  , ReadResult
  , TestWriteVectors(..)
  , WriteVector(..)
  , ReadTestWriteVectors(..)
  , ReadTestWriteResult(..)
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


writeMutableShare
  :: Backend b
  => b
  -> SlotSecrets
  -> StorageIndex
  -> ShareNumber
  -> ShareData
  -> Maybe ByteRanges
  -> IO ()
writeMutableShare b secrets storageIndex shareNumber shareData Nothing = do
  let testWriteVectors =
        fromList
        [( shareNumber
         , TestWriteVectors
           { test = []
           , write =
               [WriteVector
                 { writeOffset = 0
                 , shareData = shareData
                 }
               ]
           }
         )]
  let vectors = ReadTestWriteVectors
                { secrets = secrets
                , testWriteVectors = testWriteVectors
                , readVector = mempty
                }
  result <- readvAndTestvAndWritev b storageIndex vectors
  case (success result) of
    False -> throw WriteRefused
    True -> return ()

data WriteRefused = WriteRefused deriving (Show, Eq)
instance Exception WriteRefused

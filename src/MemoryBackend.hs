module MemoryBackend
  ( MemoryBackend(MemoryBackend)
  , memoryBackend
  ) where

import Prelude hiding
  ( map
  , lookup
  , filter
  )

import Data.IORef
 ( IORef
 , newIORef
 , readIORef
 , modifyIORef
 )

import Data.Map.Strict
  ( Map
  , map
  , keys
  , lookup
  , alter
  , member
  , insert
  , adjust
  , fromList
  , filterWithKey
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

type ShareStorage = Map StorageIndex (Map ShareNumber ShareData)
type BucketStorage = Map StorageIndex (Map ShareNumber (Size, ShareData))

data MemoryBackend = MemoryBackend
  { shares :: IORef ShareStorage -- Completely written immutable shares
  , buckets :: IORef BucketStorage -- In-progress immutable share uploads
  }

instance Backend MemoryBackend where
  version backend = do
    totalSize <- totalShareSize backend
    return Version
      { applicationVersion = "(memory)"
      , parameters =
          Version1Parameters
          { maximumImmutableShareSize = 1024 * 1024 * 64
          , maximumMutableShareSize = 1024 * 1024 * 64
          , availableSpace = (1024 * 1024 * 1024) - totalSize
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

  writeImmutableShare backend storage_index share_number share_data Nothing = do
    let shares' = shares backend
    modifyIORef shares' $ addShare storage_index share_number share_data
    s <- readIORef shares'
    return ()

  adviseCorruptImmutableShare backend _ _ _ =
    return mempty

  getImmutableShareNumbers backend storage_index = do
    shares' <- readIORef $ shares backend
    return $ case lookup storage_index shares' of
      Nothing       -> []
      Just shares'' -> keys shares''

  readImmutableShares backend storageIndex [] offsets sizes = do
    shareNumbers <- getImmutableShareNumbers backend storageIndex
    case shareNumbers of
      [] ->
        return mempty
      _  ->
        readImmutableShares backend storageIndex shareNumbers offsets sizes

  readImmutableShares backend storageIndex shareNumbers [] [] = do
    shares' <- readIORef $ shares backend
    let result = case lookup storageIndex shares' of
          Nothing       -> mempty
          Just shares'' ->
            let matchingShares = filterWithKey matches shares'' in
              map (replicate 1) matchingShares
          where
            matches k v = k `elem` shareNumbers
    return result


totalShareSize :: MemoryBackend -> IO Size
totalShareSize backend = do
  shares <- readIORef $ shares backend
  return $ toInteger $ sum $ map length shares

addShare :: StorageIndex -> ShareNumber -> ShareData -> ShareStorage -> ShareStorage
addShare storageIndex shareNumber shareData shareStorage =
  case lookup storageIndex shareStorage of
    Nothing      ->
      insert storageIndex (fromList [(shareNumber, shareData)]) shareStorage

    Just shares  ->
      adjust addShare' storageIndex shareStorage
  where
    addShare' shares =
      insert shareNumber shareData shares

memoryBackend :: IO MemoryBackend
memoryBackend = do
  shares <- newIORef mempty
  buckets <- newIORef mempty
  return $ MemoryBackend shares buckets

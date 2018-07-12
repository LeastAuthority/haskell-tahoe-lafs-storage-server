module MemoryBackend
  ( MemoryBackend(MemoryBackend)
  , memoryBackend
  ) where

import Prelude hiding
  ( map
  , lookup
  , filter
  )

import Control.Exception
  ( throwIO
  )

import Data.IORef
 ( IORef
 , newIORef
 , readIORef
 , modifyIORef
 , atomicModifyIORef'
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
  , toList
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
  , ReadTestWriteVectors(..)
  , ReadTestWriteResult(..)
  , TestWriteVectors(..)
  , WriteVector(..)
  , shareNumbers
  )

import TahoeLAFS.Storage.Backend
  ( Backend(..)
  , ImmutableShareAlreadyWritten(ImmutableShareAlreadyWritten)
  )

type ShareStorage = Map StorageIndex (Map ShareNumber ShareData)
type BucketStorage = Map StorageIndex (Map ShareNumber (Size, ShareData))

data MemoryBackend = MemoryBackend
  { immutableShares :: IORef ShareStorage -- Completely written immutable shares
  , mutableShares :: IORef ShareStorage -- Completely written mutable shares
  , buckets :: IORef BucketStorage -- In-progress immutable share uploads
  }

instance Show MemoryBackend where
  show _ = "<MemoryBackend>"

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

  createMutableStorageIndex backend storageIndex params =
    return AllocationResult
    { alreadyHave = mempty
    , allocated = (shareNumbers params)
    }

  getMutableShareNumbers backend storageIndex = do
    shares' <- readIORef $ mutableShares backend
    return $ case lookup storageIndex shares' of
      Nothing       -> []
      Just shares'' -> keys shares''

  readvAndTestvAndWritev
    backend
    storageIndex
    (ReadTestWriteVectors secrets testWritev readv) = do
    -- TODO implement readv and testv parts.  implement secrets part.
    let shares = mutableShares backend
    modifyIORef shares $ addShares storageIndex (shares' testWritev)
    return ReadTestWriteResult
      { success = True
      , readData = mempty
      }
      where
        shares'
          :: Map ShareNumber TestWriteVectors
          -> [(ShareNumber, ShareData)]
        shares' testWritevs =
          [ (shareNumber, shareData writev)
          | (shareNumber, testWritev) <- toList testWritevs,
            writev                    <- write testWritev
          ]

  createImmutableStorageIndex backend idx params =
    return AllocationResult
    { alreadyHave = mempty
    , allocated = (shareNumbers params)
    }

  writeImmutableShare backend storageIndex shareNumber shareData Nothing = do
    shares <- readIORef (immutableShares backend)
    changed <- atomicModifyIORef' (immutableShares backend) $
      \shares -> do
        case lookup storageIndex shares >>= lookup shareNumber of
          Just _ ->
            -- It is not allowed to write new data for an immutable share that
            -- has already been written.
            (shares, False)
          Nothing ->
            (addShares storageIndex [(shareNumber, shareData)] shares, True)
    if changed
      then return ()
      else throwIO ImmutableShareAlreadyWritten

  adviseCorruptImmutableShare backend _ _ _ =
    return mempty

  getImmutableShareNumbers backend storageIndex = do
    shares' <- readIORef $ immutableShares backend
    return $ case lookup storageIndex shares' of
      Nothing       -> []
      Just shares'' -> keys shares''

  readImmutableShares backend storageIndex shareNumbers [] [] = do
    shares' <- readIORef $ immutableShares backend
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
  imm <- readIORef $ immutableShares backend
  mut <- readIORef $ mutableShares backend
  let immSize = sum $ map length imm
  let mutSize = sum $ map length mut
  return $ toInteger $ immSize + mutSize

addShares :: StorageIndex -> [(ShareNumber, ShareData)] -> ShareStorage -> ShareStorage
addShares storageIndex [] shareStorage = shareStorage
addShares storageIndex ((shareNumber, shareData):rest) shareStorage =
  let added = case lookup storageIndex shareStorage of
                Nothing      ->
                  insert storageIndex (fromList [(shareNumber, shareData)]) shareStorage

                Just shares  ->
                  adjust addShare' storageIndex shareStorage
                  where
                    addShare' shares =
                      insert shareNumber shareData shares
  in
    addShares storageIndex rest added

memoryBackend :: IO MemoryBackend
memoryBackend = do
  immutableShares <- newIORef mempty
  mutableShares <- newIORef mempty
  buckets <- newIORef mempty
  return $ MemoryBackend immutableShares mutableShares buckets

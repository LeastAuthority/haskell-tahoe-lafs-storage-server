module FilesystemBackend
  ( FilesystemBackend(FilesystemBackend)
  , storageStartSegment
  , partitionM
  , pathOf
  , incomingPathOf
  ) where

import Text.Printf
  ( printf
  )

import Data.List
  ( partition
  )

import System.FilePath
  ( FilePath
  , (</>)
  )
import System.Posix.StatVFS
  ( StatVFS(statVFS_bsize, statVFS_bavail)
  , statVFS
  )

import System.Directory
  ( doesPathExist
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
  , AllocateBuckets(..)
  , AllocationResult(..)
  )

import qualified Storage

import Backend
  ( Backend(..)
  )

data FilesystemBackend = FilesystemBackend FilePath
  deriving (Show)

versionString = "tahoe-lafs (gbs) 0.1.0"

-- Copied from the Python implementation.  Kind of arbitrary.
maxMutableShareSize = 69105 * 1000 * 1000 * 1000 * 1000

--  storage/
--  storage/shares/incoming
--    incoming/ holds temp dirs named $START/$STORAGEINDEX/$SHARENUM which will
--    be moved to storage/shares/$START/$STORAGEINDEX/$SHARENUM upon success
--  storage/shares/$START/$STORAGEINDEX
--  storage/shares/$START/$STORAGEINDEX/$SHARENUM

--  Where "$START" denotes the first 10 bits worth of $STORAGEINDEX (that's 2
--  base-32 chars).

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

  createImmutableStorageIndex backend storageIndex params = do
    let exists = haveShare backend storageIndex
    (alreadyHave, allocated) <- partitionM exists (shareNumbers params)
    putStrLn $ printf "Partitioned %s have: %s allocating %s" (show $ shareNumbers params) (show alreadyHave) (show allocated)
    putStrLn "allocating"
    allocatev backend storageIndex allocated
    return AllocationResult
      { alreadyHave = alreadyHave
      , allocated = allocated
      }

  writeImmutableShare (FilesystemBackend path) storageIndex shareNumber shareData Nothing =
    return mempty

-- Does the given backend have the complete share indicated?
haveShare
  :: FilesystemBackend -- The backend to check
  -> StorageIndex      -- The storage index the share belongs to
  -> ShareNumber       -- The number of the share
  -> IO Bool           -- True if it has the share, False otherwise.
haveShare (FilesystemBackend path) storageIndex shareNumber =
  doesPathExist $ pathOf path storageIndex shareNumber

pathOf :: FilePath -> StorageIndex -> ShareNumber -> FilePath
pathOf root storageIndex shareNumber =
  root </> "shares" </> storageStartSegment storageIndex </> storageIndex </> (show $ Storage.toInteger shareNumber)

incomingPathOf :: FilePath -> StorageIndex -> ShareNumber -> FilePath
incomingPathOf root storageIndex shareNumber =
  root </> "shares" </> "incoming" </> storageStartSegment storageIndex </> storageIndex </> (show $ Storage.toInteger shareNumber)

storageStartSegment :: StorageIndex -> FilePath
storageStartSegment [] = fail "illegal short storage index"
storageStartSegment (a:[]) = storageStartSegment []
storageStartSegment (a:b:rest) = a:b:[]

-- Create a space to write data for an incoming share.
allocate
  :: FilesystemBackend
  -> StorageIndex
  -> ShareNumber
  -> IO ()
allocate backend storageIndex shareNumber =
  allocatev backend storageIndex [shareNumber]

-- Create spaces to write data for several incoming shares.
allocatev
  :: FilesystemBackend
  -> StorageIndex
  -> [ShareNumber]
  -> IO ()
allocatev _ _ [] = return ()
allocatev (FilesystemBackend path) storageIndex (shareNumber:rest) = do
  let path = incomingPathOf path storageIndex shareNumber in do
    putStrLn $ printf "allocating %s" path
    writeFile path ""
    putStrLn $ printf "done"
    allocatev (FilesystemBackend path) storageIndex rest

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pred items = do
  (yes, no) <- partitionM' pred items [] []
  -- re-reverse them to maintain input order
  return (reverse yes, reverse no)
  where
    partitionM' pred [] yes no = return (yes, no)
    partitionM' pred (item:rest) yes no = do
      result <- pred item
      if result then
        partitionM' pred rest (item:yes) no
        else
        partitionM' pred rest yes (item:no)

{-# LANGUAGE OverloadedStrings #-}

module FilesystemBackend
  ( FilesystemBackend(FilesystemBackend)
  , storageStartSegment
  , partitionM
  , pathOfShare
  , incomingPathOf
  ) where

import Prelude hiding
  ( writeFile
  , readFile
  )

import Text.Printf
  ( printf
  )

import Data.ByteString
  ( writeFile
  , readFile
  , hPut
  )

import Control.Exception
  ( tryJust
  , throwIO
  )

import Data.Maybe
  ( catMaybes
  )

import Data.List
  ( partition
  )

import Data.Map.Strict
  ( fromList
  , toList
  )

import System.IO
  ( Handle
  , IOMode(ReadWriteMode)
  , SeekMode(AbsoluteSeek)
  , withBinaryFile
  , hSeek
  )
import System.IO.Error
  ( isDoesNotExistError
  )

import System.FilePath
  ( FilePath
  , takeDirectory
  , (</>)
  )
import System.Posix.StatVFS
  ( StatVFS(statVFS_bsize, statVFS_bavail)
  , statVFS
  )

import System.Directory
  ( doesPathExist
  , createDirectoryIfMissing
  , renameFile
  , listDirectory
  )

import Storage
  ( Version(..)
  , Size
  , Offset
  , StorageIndex
  , ShareNumber
  , ShareData
  , WriteVector(WriteVector)
  , TestWriteVectors(write)
  , ReadTestWriteVectors(ReadTestWriteVectors)
  , ReadTestWriteResult(ReadTestWriteResult, success, readData)
  , ApplicationVersion(..)
  , Version1Parameters(..)
  , AllocateBuckets(..)
  , AllocationResult(..)
  , shareNumber
  )

import qualified Storage

import Backend
  ( ImmutableShareAlreadyWritten(ImmutableShareAlreadyWritten)
  , Backend(..)
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
    allocatev backend storageIndex allocated
    return AllocationResult
      { alreadyHave = alreadyHave
      , allocated = allocated
      }

  -- TODO Handle ranges.
  -- TODO Make sure the share storage was allocated.
  -- TODO Don't allow target of rename to exist.
  -- TODO Concurrency
  writeImmutableShare (FilesystemBackend root) storageIndex shareNumber shareData Nothing = do
    alreadyHave <- haveShare (FilesystemBackend root) storageIndex shareNumber
    if alreadyHave
      then throwIO ImmutableShareAlreadyWritten
      else
      do
        let finalSharePath = pathOfShare root storageIndex shareNumber
        let incomingSharePath = incomingPathOf root storageIndex shareNumber
        writeFile incomingSharePath shareData
        let createParents = True
        createDirectoryIfMissing createParents $ takeDirectory finalSharePath
        renameFile incomingSharePath finalSharePath

  getImmutableShareNumbers (FilesystemBackend root) storageIndex = do
    let storageIndexPath = pathOfStorageIndex root storageIndex
    storageIndexChildren <-
      tryJust (Just . isDoesNotExistError) $ listDirectory storageIndexPath
    let sharePaths =
          case storageIndexChildren of
            Left _           -> []
            Right children   -> children
    return $ catMaybes $ map (shareNumber . read) sharePaths

  -- TODO Handle ranges.
  -- TODO Make sure the share storage was allocated.
  readImmutableShares (FilesystemBackend root) storageIndex shareNumbers [] [] =
    let storageIndexPath = pathOfStorageIndex root storageIndex
        only x = [x]
        readShare = readFile . (pathOfShare root storageIndex) in
      do
        allShareData <- sequence $ map readShare shareNumbers
        return $ fromList $ zip shareNumbers (map only allShareData)

  createMutableStorageIndex = createImmutableStorageIndex

  getMutableShareNumbers = getImmutableShareNumbers

  readvAndTestvAndWritev
    (FilesystemBackend root)
    storageIndex
    (ReadTestWriteVectors secrets testWritev readv) = do
    -- TODO implement readv and testv parts.  implement secrets part.
    mapM_ (applyWriteVectors root storageIndex) $ toList testWritev
    return ReadTestWriteResult
      { success = True
      , readData = mempty
      }

    where
      applyWriteVectors
        :: FilePath
        -> StorageIndex
        -> (ShareNumber, TestWriteVectors)
        -> IO ()
      applyWriteVectors root storageIndex (shareNumber, testWriteVectors) =
        mapM_ (applyShareWrite root storageIndex shareNumber) (write testWriteVectors)

      applyShareWrite
        :: FilePath
        -> StorageIndex
        -> ShareNumber
        -> WriteVector
        -> IO ()
      applyShareWrite root storageIndex shareNumber (WriteVector offset shareData) =
        let sharePath = pathOfShare root storageIndex shareNumber
            createParents = True
        in
          do
            createDirectoryIfMissing createParents $ takeDirectory sharePath
            withBinaryFile sharePath ReadWriteMode (writeAtPosition offset shareData)
        where
          writeAtPosition
            :: Offset
            -> ShareData
            -> Handle
            -> IO ()
          writeAtPosition offset shareData handle = do
            hSeek handle AbsoluteSeek offset
            hPut handle shareData



-- Does the given backend have the complete share indicated?
haveShare
  :: FilesystemBackend -- The backend to check
  -> StorageIndex      -- The storage index the share belongs to
  -> ShareNumber       -- The number of the share
  -> IO Bool           -- True if it has the share, False otherwise.
haveShare (FilesystemBackend path) storageIndex shareNumber =
  doesPathExist $ pathOfShare path storageIndex shareNumber

pathOfStorageIndex
  :: FilePath      -- The storage backend root path
  -> StorageIndex  -- The storage index to consider
  -> FilePath      -- The path to the directory containing shares for the
                   -- storage index.
pathOfStorageIndex root storageIndex =
  root </> "shares" </> storageStartSegment storageIndex </> storageIndex

pathOfShare :: FilePath -> StorageIndex -> ShareNumber -> FilePath
pathOfShare root storageIndex shareNumber =
  pathOfStorageIndex root storageIndex </> (show $ Storage.toInteger shareNumber)

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
allocatev backend storageIndex [] = return ()
allocatev (FilesystemBackend root) storageIndex (shareNumber:rest) =
  let sharePath = incomingPathOf root storageIndex shareNumber
      shareDirectory = takeDirectory sharePath
      createParents = True
  in
    do
      createDirectoryIfMissing createParents shareDirectory
      writeFile sharePath ""
      allocatev (FilesystemBackend root) storageIndex rest
      return ()

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

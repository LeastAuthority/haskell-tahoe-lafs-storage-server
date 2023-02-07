{-# LANGUAGE OverloadedStrings #-}

module TahoeLAFS.Storage.Backend.Filesystem (
    FilesystemBackend (FilesystemBackend),
    storageStartSegment,
    partitionM,
    pathOfShare,
    incomingPathOf,
) where

import Prelude hiding (
    readFile,
    writeFile,
 )

import Network.HTTP.Types (
    ByteRanges,
 )

import Data.ByteString (
    hPut,
    readFile,
    writeFile,
 )

import Control.Exception (
    throwIO,
    tryJust,
 )

import Data.Maybe (
    mapMaybe,
 )

import Data.Map.Strict (
    fromList,
    toList,
 )

import System.IO (
    Handle,
    IOMode (ReadWriteMode),
    SeekMode (AbsoluteSeek),
    hSeek,
    withBinaryFile,
 )
import System.IO.Error (
    isDoesNotExistError,
 )

import System.FilePath (
    takeDirectory,
    (</>),
 )

import System.Posix.StatVFS (
    StatVFS (statVFS_bavail, statVFS_bsize),
    statVFS,
 )

import System.Directory (
    createDirectoryIfMissing,
    doesPathExist,
    listDirectory,
    renameFile,
 )

import TahoeLAFS.Storage.API (
    AllocateBuckets (..),
    AllocationResult (..),
    LeaseSecret,
    Offset,
    ReadTestWriteResult (ReadTestWriteResult, readData, success),
    ReadTestWriteVectors (ReadTestWriteVectors),
    ShareData,
    ShareNumber,
    StorageIndex,
    TestWriteVectors (write),
    Version (..),
    Version1Parameters (..),
    WriteVector (WriteVector),
    shareNumber,
 )

import qualified TahoeLAFS.Storage.API as Storage

import TahoeLAFS.Storage.Backend (
    Backend (..),
    ImmutableShareAlreadyWritten (ImmutableShareAlreadyWritten),
 )

data FilesystemBackend = FilesystemBackend FilePath
    deriving (Show)

versionString :: Storage.ApplicationVersion
versionString = "tahoe-lafs (gbs) 0.1.0"

-- Copied from the Python implementation.  Kind of arbitrary.
maxMutableShareSize :: Storage.Size
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
        let available = toInteger (statVFS_bsize vfs) * toInteger (statVFS_bavail vfs)
        return
            Version
                { applicationVersion = versionString
                , parameters =
                    Version1Parameters
                        { maximumImmutableShareSize = available
                        , maximumMutableShareSize = maxMutableShareSize
                        , -- TODO: Copy the "reserved space" feature of the Python
                          -- implementation.
                          availableSpace = available
                        , toleratesImmutableReadOverrun = True
                        , deleteMutableSharesWithZeroLengthWritev = True
                        , fillsHolesWithZeroBytes = True
                        , preventsReadPastEndOfShareData = True
                        , -- TODO Doesn't really belong here.  Also we need more than a bool.
                          -- We need to tell them *where* it is available or it is useless.
                          httpProtocolAvailable = True
                        }
                }

    createImmutableStorageIndex :: FilesystemBackend -> StorageIndex -> AllocateBuckets -> IO AllocationResult
    createImmutableStorageIndex backend storageIndex params = do
        let exists = haveShare backend storageIndex
        (alreadyHave, allocated) <- partitionM exists (shareNumbers params)
        allocatev backend storageIndex allocated
        return
            AllocationResult
                { alreadyHave = alreadyHave
                , allocated = allocated
                }

    -- TODO Handle ranges.
    -- TODO Make sure the share storage was allocated.
    -- TODO Don't allow target of rename to exist.
    -- TODO Concurrency
    writeImmutableShare :: FilesystemBackend -> StorageIndex -> ShareNumber -> ShareData -> Maybe ByteRanges -> IO ()
    writeImmutableShare (FilesystemBackend root) storageIndex shareNumber' shareData Nothing = do
        alreadyHave <- haveShare (FilesystemBackend root) storageIndex shareNumber'
        if alreadyHave
            then throwIO ImmutableShareAlreadyWritten
            else do
                let finalSharePath = pathOfShare root storageIndex shareNumber'
                let incomingSharePath = incomingPathOf root storageIndex shareNumber'
                writeFile incomingSharePath shareData
                let createParents = True
                createDirectoryIfMissing createParents $ takeDirectory finalSharePath
                renameFile incomingSharePath finalSharePath

    getImmutableShareNumbers :: FilesystemBackend -> StorageIndex -> IO [ShareNumber]
    getImmutableShareNumbers (FilesystemBackend root) storageIndex = do
        let storageIndexPath = pathOfStorageIndex root storageIndex
        storageIndexChildren <-
            tryJust (Just . isDoesNotExistError) $ listDirectory storageIndexPath
        let sharePaths =
                case storageIndexChildren of
                    Left _ -> []
                    Right children -> children
        return $ mapMaybe (shareNumber . read) sharePaths

    -- TODO Handle ranges.
    -- TODO Make sure the share storage was allocated.
    readImmutableShares :: FilesystemBackend -> StorageIndex -> [ShareNumber] -> [Offset] -> [Storage.Size] -> IO Storage.ReadResult
    readImmutableShares (FilesystemBackend root) storageIndex shareNumbers [] [] =
        let _storageIndexPath = pathOfStorageIndex root storageIndex
            only x = [x]
            readShare = readFile . pathOfShare root storageIndex
         in fromList . zip shareNumbers . map only <$> mapM readShare shareNumbers
    readImmutableShares _ _ _ _ _ = error "readImmutableShares got bad input"

    createMutableStorageIndex = createImmutableStorageIndex

    getMutableShareNumbers = getImmutableShareNumbers
    renewLease :: FilesystemBackend -> StorageIndex -> [LeaseSecret] -> IO ()
    renewLease (FilesystemBackend root) _ _ = pure mempty

    readvAndTestvAndWritev
        (FilesystemBackend root)
        storageIndex
        (ReadTestWriteVectors _secrets testWritev _readv) = do
            -- TODO implement readv and testv parts.  implement secrets part.
            mapM_ (applyWriteVectors root storageIndex) $ toList testWritev
            return
                ReadTestWriteResult
                    { success = True
                    , readData = mempty
                    }
          where
            applyWriteVectors ::
                FilePath ->
                StorageIndex ->
                (ShareNumber, TestWriteVectors) ->
                IO ()
            applyWriteVectors _root _storageIndex (shareNumber', testWriteVectors) =
                mapM_ (applyShareWrite root storageIndex shareNumber') (write testWriteVectors)

            applyShareWrite ::
                FilePath ->
                StorageIndex ->
                ShareNumber ->
                WriteVector ->
                IO ()
            applyShareWrite _root _storageIndex shareNumber' (WriteVector offset shareData) =
                let sharePath = pathOfShare root storageIndex shareNumber'
                    createParents = True
                 in do
                        createDirectoryIfMissing createParents $ takeDirectory sharePath
                        withBinaryFile sharePath ReadWriteMode (writeAtPosition offset shareData)
              where
                writeAtPosition ::
                    Offset ->
                    ShareData ->
                    Handle ->
                    IO ()
                writeAtPosition _offset shareData' handle = do
                    hSeek handle AbsoluteSeek offset
                    hPut handle shareData'

-- Does the given backend have the complete share indicated?
haveShare ::
    FilesystemBackend -> -- The backend to check
    StorageIndex -> -- The storage index the share belongs to
    ShareNumber -> -- The number of the share
    IO Bool -- True if it has the share, False otherwise.
haveShare (FilesystemBackend path) storageIndex shareNumber' =
    doesPathExist $ pathOfShare path storageIndex shareNumber'

pathOfStorageIndex ::
    FilePath -> -- The storage backend root path
    StorageIndex -> -- The storage index to consider
    FilePath -- The path to the directory containing shares for the
    -- storage index.
pathOfStorageIndex root storageIndex =
    root </> "shares" </> storageStartSegment storageIndex </> storageIndex

pathOfShare :: FilePath -> StorageIndex -> ShareNumber -> FilePath
pathOfShare root storageIndex shareNumber' =
    pathOfStorageIndex root storageIndex </> show (Storage.toInteger shareNumber')

incomingPathOf :: FilePath -> StorageIndex -> ShareNumber -> FilePath
incomingPathOf root storageIndex shareNumber' =
    root </> "shares" </> "incoming" </> storageStartSegment storageIndex </> storageIndex </> show (Storage.toInteger shareNumber')

storageStartSegment :: StorageIndex -> FilePath
storageStartSegment [] = fail "illegal short storage index"
storageStartSegment [_] = storageStartSegment []
storageStartSegment (a : b : _) = [a, b]

-- Create a space to write data for an incoming share.
allocate ::
    FilesystemBackend ->
    StorageIndex ->
    ShareNumber ->
    IO ()
allocate backend storageIndex shareNumber' =
    allocatev backend storageIndex [shareNumber']

-- Create spaces to write data for several incoming shares.
allocatev ::
    FilesystemBackend ->
    StorageIndex ->
    [ShareNumber] ->
    IO ()
allocatev _backend _storageIndex [] = return ()
allocatev (FilesystemBackend root) storageIndex (shareNumber : rest) =
    let sharePath = incomingPathOf root storageIndex shareNumber
        shareDirectory = takeDirectory sharePath
        createParents = True
     in do
            createDirectoryIfMissing createParents shareDirectory
            writeFile sharePath ""
            allocatev (FilesystemBackend root) storageIndex rest
            return ()

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pred' items = do
    (yes, no) <- partitionM' pred' items [] []
    -- re-reverse them to maintain input order
    return (reverse yes, reverse no)
  where
    partitionM' _ [] yes no = return (yes, no)
    partitionM' pred'' (item : rest) yes no = do
        result <- pred'' item
        if result
            then partitionM' pred'' rest (item : yes) no
            else partitionM' pred'' rest yes (item : no)

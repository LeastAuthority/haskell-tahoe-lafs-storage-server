module TahoeLAFS.Storage.Server (
    StorageServerConfig (StorageServerConfig),
    app,
    main,
) where

import Control.Monad.IO.Class (
    liftIO,
 )

import Control.Exception (
    Exception,
    throw,
 )

import TahoeLAFS.Storage.API (
    AllocateBuckets,
    AllocationResult (..),
    CorruptionDetails,
    Offset,
    ReadResult,
    ReadTestWriteResult (..),
    ReadTestWriteVectors,
    ShareData,
    ShareNumber,
    Size,
    StorageAPI,
    StorageIndex,
    Version (..),
    api,
 )

import qualified TahoeLAFS.Storage.Backend as Backend
import TahoeLAFS.Storage.Backend.Filesystem (
    FilesystemBackend (FilesystemBackend),
 )

import Servant (
    Handler,
    Server,
    serve,
    (:<|>) (..),
 )

import Network.HTTP.Types (
    ByteRanges,
 )

import Network.Wai (
    Application,
 )

import Network.Wai.Handler.Warp (
    Port,
    defaultSettings,
    runSettings,
    setPort,
 )

import Network.Wai.Handler.WarpTLS (
    runTLS,
    tlsSettings,
 )

version :: Backend.Backend b => b -> Handler Version
version backend =
    liftIO (Backend.version backend)

createImmutableStorageIndex :: Backend.Backend b => b -> StorageIndex -> AllocateBuckets -> Handler AllocationResult
createImmutableStorageIndex backend storage_index params =
    liftIO (Backend.createImmutableStorageIndex backend storage_index params)

writeImmutableShare :: Backend.Backend b => b -> StorageIndex -> ShareNumber -> ShareData -> Maybe ByteRanges -> Handler ()
writeImmutableShare backend storage_index share_number share_data content_ranges =
    liftIO (Backend.writeImmutableShare backend storage_index share_number share_data content_ranges)

adviseCorruptImmutableShare :: Backend.Backend b => b -> StorageIndex -> ShareNumber -> CorruptionDetails -> Handler ()
adviseCorruptImmutableShare backend storage_index share_number details =
    liftIO (Backend.adviseCorruptImmutableShare backend storage_index share_number details)

getImmutableShareNumbers :: Backend.Backend b => b -> StorageIndex -> Handler [ShareNumber]
getImmutableShareNumbers backend storage_index =
    liftIO (Backend.getImmutableShareNumbers backend storage_index)

readImmutableShares :: Backend.Backend b => b -> StorageIndex -> [ShareNumber] -> [Offset] -> [Size] -> Handler ReadResult
readImmutableShares backend storage_index share_numbers offsets sizes =
    liftIO (Backend.readImmutableShares backend storage_index share_numbers offsets sizes)

createMutableStorageIndex :: Backend.Backend b => b -> StorageIndex -> AllocateBuckets -> Handler AllocationResult
createMutableStorageIndex backend storage_index params =
    liftIO (Backend.createMutableStorageIndex backend storage_index params)

readvAndTestvAndWritev :: Backend.Backend b => b -> StorageIndex -> ReadTestWriteVectors -> Handler ReadTestWriteResult
readvAndTestvAndWritev backend storage_index vectors =
    liftIO (Backend.readvAndTestvAndWritev backend storage_index vectors)

readMutableShares :: Backend.Backend b => b -> StorageIndex -> [ShareNumber] -> [Offset] -> [Size] -> Handler ReadResult
readMutableShares backend storage_index share_numbers offsets sizes =
    liftIO (Backend.readMutableShares backend storage_index share_numbers offsets sizes)

getMutableShareNumbers :: Backend.Backend b => b -> StorageIndex -> Handler [ShareNumber]
getMutableShareNumbers backend storage_index =
    liftIO (Backend.getMutableShareNumbers backend storage_index)

adviseCorruptMutableShare :: Backend.Backend b => b -> StorageIndex -> ShareNumber -> CorruptionDetails -> Handler ()
adviseCorruptMutableShare backend storage_index share_number details =
    liftIO (Backend.adviseCorruptMutableShare backend storage_index share_number details)

data MisconfiguredTLS = MisconfiguredTLS
    deriving (Show)
instance Exception MisconfiguredTLS

data StorageServerConfig = StorageServerConfig
    { storagePath :: FilePath
    , listenPort :: Port
    , certificate :: Maybe FilePath
    , key :: Maybe FilePath
    }
    deriving (Show, Eq)

app :: Backend.Backend b => b -> Application
app backend =
    serve api storageServer
  where
    storageServer :: Server StorageAPI
    storageServer =
        version backend
            :<|> createImmutableStorageIndex backend
            :<|> writeImmutableShare backend
            :<|> adviseCorruptImmutableShare backend
            :<|> getImmutableShareNumbers backend
            :<|> readImmutableShares backend
            :<|> createMutableStorageIndex backend
            :<|> readvAndTestvAndWritev backend
            :<|> readMutableShares backend
            :<|> getMutableShareNumbers backend
            :<|> adviseCorruptMutableShare backend

main :: StorageServerConfig -> IO ()
main config =
    run $ app (FilesystemBackend $ storagePath config)
  where
    settings = setPort (listenPort config) defaultSettings
    run a =
        case (certificate config, key config) of
            (Nothing, Nothing) -> runSettings settings a
            (Just c, Just k) -> runTLS (tlsSettings c k) settings a
            _ -> throw MisconfiguredTLS

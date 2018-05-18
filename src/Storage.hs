{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- https://artyom.me/aeson#records-and-json-generics
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Storage
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
  , ReadTestWriteVectors
  , ReadTestWriteResult(..)
  , ReadVectors
  , ReadResult
  , CorruptionDetails
  , StorageAPI
  , api
  ) where

import Data.Map.Strict
  ( Map
  )
import Data.ByteString
  ( ByteString
  )
import Data.Aeson
  ( ToJSON(..)
  , FromJSON(..)
  , defaultOptions
  , camelTo2
  , genericToJSON
  , genericParseJSON
  , fieldLabelModifier
  )
import GHC.Generics
  ( Generic
  )
import Servant
  ( Proxy(Proxy)
  , JSON
  , OctetStream
  , Capture
  , QueryParams
  , ReqBody
  , Header
  , Get
  , Post
  , Put
  , (:>)
  , (:<|>)
  )

import Web.HttpApiData
  ( FromHttpApiData(parseHeader)
  , ToHttpApiData(toHeader)
  )

import Network.HTTP.Types
  ( ByteRange
  , ByteRanges
  , parseByteRanges
  , renderByteRanges
  )

import ServantUtil
  ( CBOR
  )

tahoeJSONOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '-'
  }

type ApplicationVersion = String
type Size = Integer
type Offset = Integer
type ShareNumber = Integer
type RenewSecret = String
type CancelSecret = String
type StorageIndex = String
type ShareData = ByteString

data Version1Parameters = Version1Parameters
  { maximumImmutableShareSize                 :: Size
  , maximumMutableShareSize                   :: Size
  , availableSpace                            :: Size
  , toleratesImmutableReadOverrun             :: Bool
  , deleteMutableSharesWithZeroLengthWritev   :: Bool
  , fillsHolesWithZeroBytes                   :: Bool
  , preventsReadPastEndOfShareData            :: Bool
  , httpProtocolAvailable                     :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON Version1Parameters where
  toJSON = genericToJSON tahoeJSONOptions

instance FromJSON Version1Parameters where
  parseJSON = genericParseJSON tahoeJSONOptions

data Version = Version
  { applicationVersion :: ApplicationVersion
  , parameters         :: Version1Parameters
  } deriving (Show, Eq, Generic)

instance ToJSON Version where
  toJSON = genericToJSON tahoeJSONOptions

instance FromJSON Version where
  parseJSON = genericParseJSON tahoeJSONOptions

data AllocateBuckets = AllocateBuckets
  { renewSecret       :: RenewSecret
  , cancelSecret      :: CancelSecret
  , shareNumbers      :: [ShareNumber]
  , allocatedSize     :: Size
  } deriving (Show, Eq, Generic)

instance ToJSON AllocateBuckets where
  toJSON = genericToJSON tahoeJSONOptions

instance FromJSON AllocateBuckets where
  parseJSON = genericParseJSON tahoeJSONOptions

data AllocationResult = AllocationResult
  { alreadyHave       :: [ShareNumber]
  , allocated         :: [ShareNumber]
  } deriving (Show, Eq, Generic)

instance ToJSON AllocationResult where
  toJSON = genericToJSON tahoeJSONOptions

instance FromJSON AllocationResult where
  parseJSON = genericParseJSON tahoeJSONOptions

data ShareType =
  Mutable
  | Immutable
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CorruptionDetails = CorruptionDetails
  { reason            :: String
  } deriving (Show, Eq, Generic)

instance ToJSON CorruptionDetails where
  toJSON = genericToJSON tahoeJSONOptions

instance FromJSON CorruptionDetails where
  parseJSON = genericParseJSON tahoeJSONOptions

instance FromHttpApiData ByteRanges where
  parseHeader bs =
    case parseByteRanges bs of
      Nothing -> Left "parse failed"
      Just br -> Right br

instance ToHttpApiData ByteRanges where
  toHeader = renderByteRanges

type StorageAPI =
  -- General server information

  --
  -- GET /v1/version
  -- Retrieve information about the server version and behavior
  "v1" :> "version" :> Get '[CBOR, JSON] Version

  -- Immutable share interactions

  --
  -- POST /v1/immutable/:storage_index
  -- Initialize a new immutable storage index
  :<|> "v1" :> "immutable" :> Capture "storage_index" StorageIndex :> ReqBody '[CBOR, JSON] AllocateBuckets :> Post '[CBOR, JSON] AllocationResult

  --
  -- PUT /v1/immutable/:storage_index/:share_number
  -- Write data for an immutable share to an allocated storage index
  :<|> "v1" :> "immutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> ReqBody '[OctetStream] ShareData :> Header "Content-Range" ByteRanges :> Put '[CBOR, JSON] ()

  --
  -- POST /v1/immutable/:storage_index/:share_number/corrupt
  -- Advise the server of a corrupt share data
  :<|> "v1" :> "immutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> "corrupt" :> ReqBody '[CBOR, JSON] CorruptionDetails :> Post '[CBOR, JSON] ()

  --
  -- GET /v1/immutable/storage_index/shares
  -- Retrieve the share numbers available for a storage index
  :<|> "v1" :> "immutable" :> Capture "storage_index" StorageIndex :> "shares" :> Get '[CBOR, JSON] [ShareNumber]

  --
  -- GET /v1/immutable/:storage_index?[share=s0&share=s1&...]
  -- Read from an immutable storage index, possibly from multiple shares, possibly limited to certain ranges
  :<|> "v1" :> "immutable" :> Capture "storage_index" StorageIndex :> QueryParams "share" ShareNumber :> QueryParams "offset" Offset :> QueryParams "size" Size :> Get '[CBOR] ReadResult

  -- Mutable share interactions

  --
  -- POST /v1/mutable/:storage_index
  -- Initialize a new mutable storage index
  :<|> "v1" :> "mutable" :> Capture "storage_index" StorageIndex :> ReqBody '[CBOR, JSON] AllocateBuckets :> Post '[CBOR, JSON] AllocationResult

  --
  -- POST /v1/mutable/:storage_index/read-test-write
  -- General purpose read-test-and-write operation.
  :<|> "v1" :> "mutable" :> Capture "storage_index" StorageIndex :> "read-test-write" :> ReqBody '[CBOR, JSON] ReadTestWriteVectors :> Post '[CBOR, JSON] ReadTestWriteResult

  --
  -- GET /v1/mutable/:storage_index
  -- Read from a mutable storage index
  :<|> "v1" :> "mutable" :> Capture "storage_index" StorageIndex :> QueryParams "share" ShareNumber :> QueryParams "offset" Offset :> QueryParams "size" Size :> Get '[CBOR, JSON] ReadResult

  --
  -- GET /v1/mutable/:storage_index/shares
  -- Retrieve the share numbers available for a storage index
  :<|> "v1" :> "mutable" :> Capture "storage_index" StorageIndex :> "shares" :> Get '[CBOR, JSON] [ShareNumber]

  --
  -- POST /v1/mutable/:storage_index/:share_number/corrupt
  -- Advise the server of a corrupt share data
  :<|> "v1" :> "mutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> "corrupt" :> ReqBody '[CBOR, JSON] CorruptionDetails :> Post '[CBOR, JSON] ()

type ReadResult = Map ShareNumber [ShareData]

data ReadVectors = ReadVectors
  { shares      :: [ShareNumber]
  , readVectors :: [ReadVector]
  } deriving (Show, Eq, Generic)

instance ToJSON ReadVectors where
  toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadVectors where
  parseJSON = genericParseJSON tahoeJSONOptions

data ReadTestWriteResult = ReadTestWriteResult
  { success  :: Bool
  , readData :: ReadResult
  } deriving (Show, Eq, Generic)

instance ToJSON ReadTestWriteResult where
  toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadTestWriteResult where
  parseJSON = genericParseJSON tahoeJSONOptions

data ReadTestWriteVectors = ReadTestWriteVectors
  { secrets          :: SlotSecrets
  , testWriteVectors :: Map ShareNumber TestWriteVectors
  , readVector       :: [ReadVector]
  } deriving (Show, Eq, Generic)

instance ToJSON ReadTestWriteVectors where
  toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadTestWriteVectors where
  parseJSON = genericParseJSON tahoeJSONOptions

data ReadVector = ReadVector
  { offset     :: Offset
  , readSize   :: Size
  } deriving (Show, Eq, Generic)

instance ToJSON ReadVector where
  toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadVector where
  parseJSON = genericParseJSON tahoeJSONOptions

data TestWriteVectors = TestWriteVectors
  { test  :: [TestVector]
  , write :: [WriteVector]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TestOperator =
  Lt
  | Le
  | Eq
  | Ne
  | Ge
  | Gt
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TestVector = TestVector
  { testOffset :: Offset
  , testSize   :: Size
  , operator   :: TestOperator
  , specimen   :: ShareData
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data WriteVector = WriteVector
  { writeOffset :: Offset
  , shareData   :: ShareData
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SlotSecrets = SlotSecrets
  { writeEnabler :: WriteEnablerSecret
  , leaseRenew   :: LeaseRenewSecret
  , leaseCancel  :: LeaseCancelSecret
  } deriving (Show, Eq, Generic)

instance ToJSON SlotSecrets where
  toJSON = genericToJSON tahoeJSONOptions

instance FromJSON SlotSecrets where
  parseJSON = genericParseJSON tahoeJSONOptions


type WriteEnablerSecret = String
type LeaseRenewSecret = String
type LeaseCancelSecret = String

api :: Proxy StorageAPI
api = Proxy

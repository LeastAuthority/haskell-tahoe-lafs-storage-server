{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- https://artyom.me/aeson#records-and-json-generics
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Storage
  ( Version(..)
  , StorageIndex
  , BucketIdentifier
  , StorageBuckets
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
type BucketIdentifier = String
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

type StorageBuckets = Map ShareNumber BucketIdentifier

data AllocationResult = AllocationResult
  { alreadyHave       :: [ShareNumber]
  , allocated         :: StorageBuckets
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
  { shareType         :: ShareType
  , storageIndex      :: StorageIndex
  , reason            :: String
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
  "v1" :> "version" :> Get '[CBOR, JSON] Version

  -- Share interactions
  -- Allocate buckets for share writing
  :<|> "v1" :> "storage" :> Capture "storage_index" StorageIndex :> ReqBody '[CBOR, JSON] AllocateBuckets :> Post '[CBOR, JSON] AllocationResult
  -- Retrieve the bucket identifiers for a storage index
  :<|> "v1" :> "storage" :> Capture "storage_index" StorageIndex :> Get '[CBOR, JSON] StorageBuckets

  -- Write share data to an allocated bucket
  :<|> "v1" :> "buckets" :> Capture "bucket_id" BucketIdentifier :> ReqBody '[OctetStream] ShareData :> Header "Content-Range" ByteRanges :> Put '[CBOR, JSON] ()
  -- Read share data from a previously written bucket
  :<|> "v1" :> "buckets" :> Capture "bucket_id" BucketIdentifier :> Get '[OctetStream] ShareData
  -- Advise the server of a corrupt bucket contents
  :<|> "v1" :> "buckets" :> Capture "bucket_id" BucketIdentifier :> "corrupt" :> ReqBody '[CBOR, JSON] CorruptionDetails :> Post '[CBOR, JSON] ()

  -- Slot interactions
  -- Write to a slot
  :<|> "v1" :> "slots" :> Capture "storage_index" StorageIndex :> ReqBody '[CBOR, JSON] ReadTestWriteVectors :> Post '[CBOR, JSON] ReadTestWriteResult
  -- Read from a slot
  :<|> "v1" :> "slots" :> Capture "storage_index" StorageIndex :> ReqBody '[CBOR, JSON] ReadVectors :> Post '[CBOR, JSON] ReadResult

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

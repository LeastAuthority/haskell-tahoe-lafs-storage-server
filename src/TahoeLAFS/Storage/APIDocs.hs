{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TahoeLAFS.Storage.APIDocs where

import Prelude hiding (
    Eq,
 )

import Data.Bits (
    shiftL,
 )

import Data.Map (
    Map,
    fromList,
 )

import Servant (
    Capture (..),
    Optional,
    QueryParams,
 )

import Servant.Docs (
    DocCapture (DocCapture),
    DocQueryParam (DocQueryParam),
    ParamKind (List, Normal),
    ToCapture (toCapture),
    ToParam (toParam),
    ToSample (toSamples),
    samples,
    singleSample,
 )

import TahoeLAFS.Storage.API (
    AllocateBuckets (AllocateBuckets),
    AllocationResult (AllocationResult),
    ApplicationVersion,
    CorruptionDetails (CorruptionDetails),
    Offset,
    ReadResult,
    ReadTestWriteResult (ReadTestWriteResult),
    ReadTestWriteVectors (ReadTestWriteVectors),
    ReadVector,
    ShareData,
    ShareNumber (ShareNumber),
    SlotSecrets (SlotSecrets),
    StorageIndex,
    TestOperator (Eq),
    TestVector (TestVector),
    TestWriteVectors (TestWriteVectors),
    Version (Version),
    Version1Parameters (Version1Parameters),
    WriteVector (WriteVector),
    leaseCancelSecretLength,
    leaseRenewSecretLength,
    renewSecretLength,
    writeEnablerSecretLength,
 )

instance ToCapture (Capture "storage_index" StorageIndex) where
    toCapture _ = DocCapture "storage index" "(hex string) a storage index to use to address the data"

instance ToCapture (Capture "share_number" ShareNumber) where
    toCapture _ = DocCapture "share number" "(integer) a share number to use to address a particular share"

instance ToParam (QueryParams "share_number" ShareNumber) where
    toParam _ = DocQueryParam "share_number" [] "(integer) a share number to use to address a particular share" List

instance ToParam (QueryParams "offset" Integer) where
    toParam _ = DocQueryParam "offset" [] "(integer) offset into a share to read or write" List

instance ToParam (QueryParams "size" Integer) where
    toParam _ = DocQueryParam "size" [] "(integer) number of bytes of a share to read" List

instance ToSample ReadResult where
    toSamples _ = singleSample mempty

instance ToSample Version where
    toSamples _ =
        singleSample $
            Version "blub version??" (Version1Parameters (1 `shiftL` 16) (2 `shiftL` 32) (2 `shiftL` 64) True True True True True)

instance ToSample AllocateBuckets where
    toSamples _ =
        singleSample
            ( AllocateBuckets
                (example renewSecretLength "a")
                (example renewSecretLength "b")
                [ShareNumber 1, ShareNumber 3]
                1024
            )

instance ToSample AllocationResult where
    toSamples _ =
        singleSample $ AllocationResult [ShareNumber 1] [ShareNumber 3]

instance ToSample ShareData where
    toSamples _ =
        singleSample "abcdefgh"

instance ToSample () where
    toSamples _ = singleSample ()

instance ToSample CorruptionDetails where
    toSamples _ = singleSample $ CorruptionDetails "sha256 mismatch maybe?"

instance ToSample ShareNumber where
    toSamples _ = samples [ShareNumber 0, ShareNumber 3]

instance ToSample ReadTestWriteVectors where
    toSamples _ =
        singleSample $
            ReadTestWriteVectors
                (SlotSecrets (example writeEnablerSecretLength "c") (example leaseRenewSecretLength "d") (example leaseCancelSecretLength "e"))
                sampleTestWriteVectors
                sampleReadVector

instance ToSample ReadTestWriteResult where
    toSamples _ =
        singleSample $
            ReadTestWriteResult True sampleReadResult

sampleTestWriteVectors :: Map ShareNumber TestWriteVectors
sampleTestWriteVectors =
    fromList
        [(ShareNumber 0, TestWriteVectors [TestVector 32 33 Eq "x"] [WriteVector 32 "y"])]

sampleReadVector :: [ReadVector]
sampleReadVector = mempty

sampleReadResult :: ReadResult
sampleReadResult = mempty

example :: Int -> [a] -> [a]
example n s = concat $ replicate n s

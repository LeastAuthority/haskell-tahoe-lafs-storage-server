{-# LANGUAGE OverloadedStrings #-}

module Lib (
    gen10String,
    genStorageIndex,
    positiveIntegers,
    b32encode,
    b32decode,
) where

import Data.Word (
    Word8,
 )

import qualified Data.Base32String as Base32

import Data.ByteString (
    ByteString,
    pack,
 )

import qualified Data.Text as Text

import Test.QuickCheck (
    Arbitrary (arbitrary),
    Gen,
    suchThatMap,
    vectorOf,
 )

-- Get the Arbitrary ByteString instance.
import Test.QuickCheck.Instances.ByteString ()

import TahoeLAFS.Storage.API (
    ShareNumber,
    StorageIndex,
    shareNumber,
 )

gen10String :: Gen String
gen10String = vectorOf 10 arbitrary

gen10ByteString :: Gen ByteString
gen10ByteString =
    suchThatMap (vectorOf 10 (arbitrary :: Gen Word8)) (Just . pack)

genStorageIndex :: Gen StorageIndex
genStorageIndex =
    suchThatMap gen10ByteString (Just . b32encode)

positiveIntegers :: Gen Integer
positiveIntegers =
    suchThatMap (arbitrary :: Gen Integer) (Just . abs)

instance Arbitrary ShareNumber where
    arbitrary = suchThatMap positiveIntegers shareNumber

b32table :: ByteString
b32table = "abcdefghijklmnopqrstuvwxyz234567"

b32encode :: ByteString -> String
b32encode = Text.unpack . Base32.toText . Base32.fromBytes b32table

b32decode :: String -> ByteString
b32decode base32 =
    Base32.toBytes b32table $ Base32.fromText b32table $ Text.pack base32

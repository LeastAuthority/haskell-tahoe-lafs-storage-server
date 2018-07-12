module MiscSpec
  ( spec
  ) where

import Prelude hiding
  ( toInteger
  )

import Text.Printf
  ( printf
  )

import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  , shouldNotBe
  , shouldSatisfy
  )

import qualified Data.ByteString as BS

import Test.QuickCheck
  ( Gen
  , Arbitrary(arbitrary)
  , property
  , forAll
  , vectorOf
  )

import Storage
  ( toInteger
  )

-- We also get the Arbitrary ShareNumber instance from here.
import Lib
  ( genStorageIndex
  , b32encode
  , b32decode
  )

import TahoeLAFS.Storage.Backend.Filesystem
  ( partitionM
  , storageStartSegment
  , pathOfShare
  , incomingPathOf
  )

spec :: Spec
spec = do
  describe "partitionM" $
    it "handles empty lists" $
    partitionM (\e -> return True) ([]::[(Integer,Integer)]) `shouldBe` (Just ([], []))

  describe "partitionM" $
    it "puts matching elements in the first list and non-matching in the second" $
    partitionM (\e -> return $ (e `mod` 2) == 0) [5, 5, 6, 7, 8, 8]
    `shouldBe`
    (Just ([6, 8, 8], [5, 5, 7]))

  describe "storageStartSegment" $
    it "returns a string of length 2" $ property $
    forAll genStorageIndex (\storageIndex -> length (storageStartSegment storageIndex) `shouldBe` 2)

  describe "pathOfShare" $
    it "returns a path reflecting the storage index and share number" $ property $
    forAll genStorageIndex
    (\storageIndex shareNumber ->
       pathOfShare "/foo" storageIndex shareNumber
       `shouldBe`
       (printf "/foo/shares/%s/%s/%d" (take 2 storageIndex) storageIndex (toInteger shareNumber))
    )

  describe "incomingPathOf" $
    it "returns a path reflecting the storage index and share number" $ property $
    forAll genStorageIndex
    (\storageIndex shareNumber ->
       incomingPathOf "/foo" storageIndex shareNumber
       `shouldBe`
       (printf "/foo/shares/incoming/%s/%s/%d" (take 2 storageIndex) storageIndex (toInteger shareNumber))
    )

  describe "incomingPathOf vs pathOfShare" $
    it "returns different paths" $ property $
    forAll genStorageIndex
    (\storageIndex shareNumber ->
       let path = pathOfShare "/foo" storageIndex shareNumber
           incoming = incomingPathOf "/foo" storageIndex shareNumber
       in
         path `shouldNotBe` incoming
    )

  describe "base32 round-trip" $
    it "b32encode and b32decode are inverses" $ property $
    \bs -> (b32decode . b32encode) bs `shouldBe` bs

  describe "base32 alphabet" $
    it "encodes using only the base32 alphabet" $ property $
    \bs -> b32encode bs `shouldSatisfy` (onlyContains "abcdefghijklmnopqrstuvwxyz234567")

  describe "size ratio" $
    it "encodes to a string no more than twice the length" $ property $
    \bs -> b32encode bs `shouldSatisfy` (\base32 -> length base32 <= 2 * BS.length bs)

-- Does the second list contain only elements of the first list?
onlyContains :: (Eq a) => [a] -> [a] -> Bool
onlyContains xs [] = True
onlyContains xs (y:ys) = y `elem` xs && onlyContains xs ys

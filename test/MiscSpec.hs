module MiscSpec
  ( spec
  ) where

import Text.Printf
  ( printf
  )

import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
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

-- We also get the Arbitrary ShareNumber instance from here.
import Lib
  ( genStorageIndex
  , b32encode
  , b32decode
  )

import FilesystemBackend
  ( partitionM
  , storageStartSegment
  , pathOf
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

  describe "pathOf" $
    it "returns a path reflecting the storage index and share number" $ property $
    forAll genStorageIndex
    (\storageIndex shareNumber ->
       pathOf "/foo" storageIndex shareNumber
       `shouldBe`
       (printf "/foo/shares/%s/%s/%s" (take 2 storageIndex) storageIndex (show shareNumber))
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

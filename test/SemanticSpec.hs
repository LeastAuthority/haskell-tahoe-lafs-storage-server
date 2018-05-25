module SemanticSpec
  ( spec
  ) where

import Data.Bits
  ( xor
  )

import Test.Hspec
  ( Spec
  , context
  , describe
  , it
  , shouldBe
  )
import Test.QuickCheck
  ( Property
  , Arbitrary(arbitrary)
  , Gen
  , forAll
  , vectorOf
  , property
  )

import Test.QuickCheck.Monadic
  ( monadicIO
  , assert
  , run
  )

import qualified Test.QuickCheck.Instances.ByteString

import Data.ByteString
  ( ByteString
  , pack
  , map
  , concat
  , length
  )
import Data.ByteString.Char8
  ( unpack
  )

import Data.List
  ( sort
  )

import Data.Map.Strict
  ( Map
  , toList
  )

import Storage
  ( Size
  , StorageIndex
  , ShareNumber
  , ShareData
  , AllocateBuckets(AllocateBuckets)
  , alreadyHave
  , allocated
  )

import Backend
  ( Backend
    (createImmutableStorageIndex
    , writeImmutableShare
    , readImmutableShares
    )
  )

import MemoryBackend
  ( memoryBackend
  )

-- In the result of creating an immutable storage index, the sum of
-- ``alreadyHave`` and ``allocated`` equals ``shareNumbers`` from the input.
alreadyHavePlusAllocated :: Backend b => b -> StorageIndex -> [ShareNumber] -> Size -> Property
alreadyHavePlusAllocated b storage_index share_numbers size = monadicIO $ do
  result <- run $ createImmutableStorageIndex b storage_index $ AllocateBuckets "renew" "cancel" share_numbers size
  assert $ (alreadyHave result) ++ (allocated result) == share_numbers


immutableWriteAndReadShare :: Backend b => b -> StorageIndex -> [ShareNumber] -> ByteString -> Property
immutableWriteAndReadShare b storage_index share_numbers share_seed = monadicIO $ do
  let permutedShares = Prelude.map (permuteShare share_seed) share_numbers
  let size = fromIntegral (Data.ByteString.length share_seed)
  let allocate = AllocateBuckets "renew" "cancel" share_numbers size
  result <- run $ createImmutableStorageIndex b storage_index allocate
  run $ writeShares b storage_index $ zip share_numbers permutedShares
  readShares <- run $ readShares b storage_index share_numbers
  assert $ permutedShares == readShares
  where
    permuteShare :: ByteString -> ShareNumber -> ByteString
    permuteShare seed number =
      Data.ByteString.map (xor $ fromIntegral number) seed

    readShares :: Backend b => b -> StorageIndex -> [ShareNumber] -> IO [ShareData]
    readShares b storage_index share_numbers = do
      -- Map ShareNumber [ShareData]
      shares <- readImmutableShares b storage_index share_numbers [] []
      let shareList = (toList shares) :: [(ShareNumber, [ShareData])]
      let sortedShares = (sort shareList) :: [(ShareNumber, [ShareData])]
      let shareDataLists = (Prelude.map snd sortedShares) :: [[ShareData]]
      let shareData = (Prelude.map Data.ByteString.concat shareDataLists) :: [ShareData]
      return shareData

    writeShares :: Backend b => b -> StorageIndex -> [(ShareNumber, ShareData)] -> IO ()
    writeShares b storage_index [] = return ()
    writeShares b storage_index ((share_number, share_data):rest) = do
      -- TODO For now we'll do single complete writes.  Later try breaking up the data.
      writeImmutableShare b storage_index share_number share_data Nothing
      writeShares b storage_index rest


gen16String :: Gen String
gen16String = vectorOf 16 arbitrary

spec :: Spec
spec =
  context "immutable" $ do
  describe "Allocate an immutable storage index" $ do
    it "accounts for all allocated share numbers" $ property $
      forAll gen16String (alreadyHavePlusAllocated backend)

  describe "Write an immutable share" $ do
    it "returns the written data when requested" $ property $
      forAll gen16String (immutableWriteAndReadShare backend)

  where
    backend = memoryBackend

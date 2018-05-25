module SemanticSpec
  ( spec
  ) where

import Data.Bits
  ( xor
  )

import qualified Data.Set as Set

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
  , collect
  , (===)
  )

import Test.QuickCheck.Monadic
  ( monadicIO
  , assert
  , pre
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

isUnique :: Ord a => [a] -> Bool
isUnique xs = Prelude.length xs == (Prelude.length $ Set.toList $ Set.fromList xs)

-- In the result of creating an immutable storage index, the sum of
-- ``alreadyHave`` and ``allocated`` equals ``shareNumbers`` from the input.
alreadyHavePlusAllocated :: Backend b => IO b -> StorageIndex -> [ShareNumber] -> Size -> Property
alreadyHavePlusAllocated b storageIndex shareNumbers size = collect shareNumbers $ monadicIO $ do
  pre (isUnique shareNumbers)
  pre (all (>= 0) shareNumbers)
  backend <- run b
  result <- run $ createImmutableStorageIndex backend storageIndex $ AllocateBuckets "renew" "cancel" shareNumbers size
  assert $ (alreadyHave result) ++ (allocated result) == shareNumbers


immutableWriteAndReadShare :: Backend b => IO b -> StorageIndex -> [ShareNumber] -> ByteString -> Property
immutableWriteAndReadShare b storageIndex shareNumbers shareSeed = monadicIO $ do
  pre (isUnique shareNumbers)
  pre (all (>= 0) shareNumbers)
  let permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
  let size = fromIntegral (Data.ByteString.length shareSeed)
  let allocate = AllocateBuckets "renew" "cancel" shareNumbers size
  backend <- run b
  result <- run $ createImmutableStorageIndex backend storageIndex allocate
  run $ writeShares backend storageIndex $ zip shareNumbers permutedShares
  readShares <- run $ readShares backend storageIndex shareNumbers
  assert $ permutedShares == readShares
  where
    permuteShare :: ByteString -> ShareNumber -> ByteString
    permuteShare seed number =
      Data.ByteString.map (xor $ fromIntegral number) seed

    readShares :: Backend b => b -> StorageIndex -> [ShareNumber] -> IO [ShareData]
    readShares b storageIndex shareNumbers = do
      -- Map ShareNumber [ShareData]
      shares <- readImmutableShares b storageIndex shareNumbers [] []
      let shareList = (toList shares) :: [(ShareNumber, [ShareData])]
      let sortedShares = (sort shareList) :: [(ShareNumber, [ShareData])]
      let shareDataLists = (Prelude.map snd sortedShares) :: [[ShareData]]
      let shareData = (Prelude.map Data.ByteString.concat shareDataLists) :: [ShareData]
      return shareData

    writeShares :: Backend b => b -> StorageIndex -> [(ShareNumber, ShareData)] -> IO ()
    writeShares b storageIndex [] = return ()
    writeShares b storageIndex ((shareNumber, shareData):rest) = do
      -- TODO For now we'll do single complete writes.  Later try breaking up the data.
      writeImmutableShare b storageIndex shareNumber shareData Nothing
      writeShares b storageIndex rest


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

module SemanticSpec
  ( spec
  ) where

import Prelude hiding
  ( toInteger
  , lookup
  )

import Control.Monad
  ( when
  )

import Data.Maybe
  ( catMaybes
  )

import Data.Bits
  ( xor
  )

import GHC.Word
  ( Word8
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
  , suchThatMap
  , vectorOf
  , property
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
  , map
  , concat
  , length
  )

import Data.List
  ( sort
  )

import Data.Map.Strict
  ( lookup
  )

import Storage
  ( Size
  , StorageIndex
  , ShareNumber
  , ShareData
  , AllocateBuckets(AllocateBuckets)
  , alreadyHave
  , allocated
  , shareNumber
  , toInteger
  )

import Backend
  ( Backend
    (createImmutableStorageIndex
    , writeImmutableShare
    , readImmutableShares
    , getImmutableShareNumbers
    )
  )

import MemoryBackend
  ( memoryBackend
  )

positiveIntegers :: Gen Integer
positiveIntegers = suchThatMap (arbitrary :: Gen Integer) (\i -> Just $ abs i)

instance Arbitrary ShareNumber where
  arbitrary = suchThatMap positiveIntegers (\i -> shareNumber i)

isUnique :: Ord a => [a] -> Bool
isUnique xs = Prelude.length xs == (Prelude.length $ Set.toList $ Set.fromList xs)

permuteShare :: ByteString -> ShareNumber -> ByteString
permuteShare seed number =
  Data.ByteString.map xor' seed
  where
    xor' :: Word8 -> Word8
    xor' = xor $ fromInteger $ toInteger number

writeShares :: Backend b => b -> StorageIndex -> [(ShareNumber, ShareData)] -> IO ()
writeShares b storageIndex [] = return ()
writeShares b storageIndex ((shareNumber, shareData):rest) = do
  -- TODO For now we'll do single complete writes.  Later try breaking up the data.
  writeImmutableShare b storageIndex shareNumber shareData Nothing
  writeShares b storageIndex rest

-- In the result of creating an immutable storage index, the sum of
-- ``alreadyHave`` and ``allocated`` equals ``shareNumbers`` from the input.
alreadyHavePlusAllocated :: Backend b => IO b -> StorageIndex -> [ShareNumber] -> Size -> Property
alreadyHavePlusAllocated b storageIndex shareNumbers size = monadicIO $ do
  pre (isUnique shareNumbers)
  backend <- run b
  result <- run $ createImmutableStorageIndex backend storageIndex $ AllocateBuckets "renew" "cancel" shareNumbers size
  assert $ (alreadyHave result) ++ (allocated result) == shareNumbers

-- The share numbers of immutable share data written to the shares of a given
-- storage index can be retrieved.
immutableWriteAndEnumerateShares :: Backend b => IO b -> StorageIndex -> [ShareNumber] -> ByteString -> Property
immutableWriteAndEnumerateShares b storageIndex shareNumbers shareSeed = monadicIO $ do
  pre (isUnique shareNumbers)
  let permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
  let size = fromIntegral (Data.ByteString.length shareSeed)
  let allocate = AllocateBuckets "renew" "cancel" shareNumbers size
  backend <- run b
  result <- run $ createImmutableStorageIndex backend storageIndex allocate
  run $ writeShares backend storageIndex $ zip shareNumbers permutedShares
  readShareNumbers <- run $ getImmutableShareNumbers backend storageIndex
  let sreadShareNumbers = sort readShareNumbers
  let sshareNumbers = sort shareNumbers
  when (sreadShareNumbers /= sshareNumbers) $
    fail (show sreadShareNumbers ++ " /= " ++ show sshareNumbers)

-- Immutable share data written to the shares of a given storage index can be
-- retrieved verbatim and associated with the same share numbers as were
-- specified during writing.
immutableWriteAndReadShare :: Backend b => IO b -> StorageIndex -> [ShareNumber] -> ByteString -> Property
immutableWriteAndReadShare b storageIndex shareNumbers shareSeed = monadicIO $ do
  pre (isUnique shareNumbers)
  let permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
  let size = fromIntegral (Data.ByteString.length shareSeed)
  let allocate = AllocateBuckets "renew" "cancel" shareNumbers size
  backend <- run b
  result <- run $ createImmutableStorageIndex backend storageIndex allocate
  run $ writeShares backend storageIndex $ zip shareNumbers permutedShares
  readShares' <- run $ readShares backend storageIndex shareNumbers
  when (permutedShares /= readShares') $
    fail (show permutedShares ++ " /= " ++ show readShares')
  where
    readShares :: Backend b => b -> StorageIndex -> [ShareNumber] -> IO [ShareData]
    readShares b storageIndex shareNumbers = do
      -- Map ShareNumber [ShareData]
      shares <- readImmutableShares b storageIndex shareNumbers [] []
      let maybeShares = (Prelude.map (flip lookup shares) shareNumbers)
      let orderedShares = catMaybes maybeShares
      let shareData = (Prelude.map Data.ByteString.concat orderedShares) :: [ShareData]
      return shareData

gen16String :: Gen String
gen16String = vectorOf 16 arbitrary

makeSpec :: Backend a => IO a -> Spec
makeSpec backend = context "immutable" $ do
  describe "Allocate an immutable storage index" $ do
    it "accounts for all allocated share numbers" $ property $
      forAll gen16String (alreadyHavePlusAllocated backend)

  describe "Write an immutable share" $ do
    it "returns the written data when requested" $ property $
      forAll gen16String (immutableWriteAndReadShare backend)

  describe "Write an immutable share" $ do
    it "returns the share numbers that were written" $ property $
      forAll gen16String (immutableWriteAndEnumerateShares backend)

spec :: Spec
spec = makeSpec memoryBackend

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

import Control.Exception
  ( bracket
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

import System.Directory
  ( removeDirectoryRecursive
  )

import System.IO.Temp
  ( getCanonicalTemporaryDirectory
  , createTempDirectory
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
  , SlotSecrets(..)
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

    , createMutableStorageIndex
    , getMutableShareNumbers
    )
  , writeMutableShare
  )

import MemoryBackend
  ( memoryBackend
  )

import FilesystemBackend
  ( FilesystemBackend(FilesystemBackend)
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

writeShares
  :: (ShareNumber -> ShareData -> Maybe a -> IO ())
  -> [(ShareNumber, ShareData)] -> IO ()
writeShares write [] = return ()
writeShares write ((shareNumber, shareData):rest) = do
  -- TODO For now we'll do single complete writes.  Later try breaking up the data.
  write shareNumber shareData Nothing
  writeShares write rest

-- In the result of creating an immutable storage index, the sum of
-- ``alreadyHave`` and ``allocated`` equals ``shareNumbers`` from the input.
alreadyHavePlusAllocatedImm
  :: Backend b
  => IO b             -- The backend on which to operate
  -> StorageIndex     -- The storage index to use
  -> [ShareNumber]    -- The share numbers to allocate
  -> Size             -- The size of each share
  -> Property
alreadyHavePlusAllocatedImm b storageIndex shareNumbers size = monadicIO $ do
  pre (isUnique shareNumbers)
  backend <- run b
  result <- run $ createImmutableStorageIndex backend storageIndex $ AllocateBuckets "renew" "cancel" shareNumbers size
  when ((alreadyHave result) ++ (allocated result) /= shareNumbers)
    $ fail (show (alreadyHave result)
            ++ " ++ "
            ++ show (allocated result)
            ++ " /= "
            ++ (show shareNumbers)
           )

-- In the result of creating a mutable storage index, the sum of
-- ``alreadyHave`` and ``allocated`` equals ``shareNumbers`` from the input.
alreadyHavePlusAllocatedMut
  :: Backend b
  => IO b             -- The backend on which to operate
  -> StorageIndex     -- The storage index to use
  -> [ShareNumber]    -- The share numbers to allocate
  -> Size             -- The size of each share
  -> Property
alreadyHavePlusAllocatedMut b storageIndex shareNumbers size = monadicIO $ do
  pre (isUnique shareNumbers)
  backend <- run b
  result <- run $ createMutableStorageIndex backend storageIndex $ AllocateBuckets "renew" "cancel" shareNumbers size
  when ((alreadyHave result) ++ (allocated result) /= shareNumbers)
    $ fail (show (alreadyHave result)
            ++ " ++ "
            ++ show (allocated result)
            ++ " /= "
            ++ (show shareNumbers)
           )

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
  run $ writeShares (writeImmutableShare backend storageIndex) (zip shareNumbers permutedShares)
  readShareNumbers <- run $ getImmutableShareNumbers backend storageIndex
  let sreadShareNumbers = sort readShareNumbers
  let sshareNumbers = sort shareNumbers
  when (sreadShareNumbers /= sshareNumbers) $
    fail (show sreadShareNumbers ++ " /= " ++ show sshareNumbers)

-- The share numbers of mutable share data written to the shares of a given
-- storage index can be retrieved.
mutableWriteAndEnumerateShares :: Backend b => IO b -> StorageIndex -> [ShareNumber] -> ByteString -> Property
mutableWriteAndEnumerateShares b storageIndex shareNumbers shareSeed = monadicIO $ do
  pre (isUnique shareNumbers)
  let permutedShares = Prelude.map (permuteShare shareSeed) shareNumbers
  let size = fromIntegral (Data.ByteString.length shareSeed)
  let allocate = AllocateBuckets "renew" "cancel" shareNumbers size
  let nullSecrets = SlotSecrets
                    { writeEnabler = ""
                    , leaseRenew = ""
                    , leaseCancel = ""
                    }
  backend <- run b
  result <- run $ createMutableStorageIndex backend storageIndex allocate
  run $ writeShares (writeMutableShare backend nullSecrets storageIndex) (zip shareNumbers permutedShares)
  readShareNumbers <- run $ getMutableShareNumbers backend storageIndex
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
  run $ writeShares (writeImmutableShare backend storageIndex) (zip shareNumbers permutedShares)
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
makeSpec backend =
  context "v1" $ do

  context "immutable" $ do
    describe "allocate a storage index" $ do
      it "accounts for all allocated share numbers" $ property $
        forAll gen16String (alreadyHavePlusAllocatedImm backend)

    describe "write a share" $ do
      it "returns the share numbers that were written" $ property $
        forAll gen16String (immutableWriteAndEnumerateShares backend)

    describe "write a share" $ do
      it "returns the written data when requested" $ property $
        forAll gen16String (immutableWriteAndReadShare backend)

  context "mutable" $ do
    describe "allocate a storage index" $ do
      it "accounts for all allocated share numbers" $ property $
        forAll gen16String (alreadyHavePlusAllocatedMut backend)

    describe "write a share" $ do
      it "returns the share numbers that were written" $ property $
        forAll gen16String (mutableWriteAndEnumerateShares backend)

spec :: Spec
spec = context "backends" $
  context "in-memory" $ (makeSpec memoryBackend)

  context "filesystem" $ around withTemporaryDirectory $ \dirpath -> do
  (makeSpec $ FilesystemBackend dirpath)

withTemporaryDirectory :: (FilePath -> IO ()) -> IO ()
withTemporaryDirectory =
  bracket createTemporaryDirectory removeDirectoryRecursive
  where
    createTemporaryDirectory :: IO FilePath
    createTemporaryDirectory = do
      parent <- getCanonicalTemporaryDirectory
      createTempDirectory parent "gbs-semanticspec"

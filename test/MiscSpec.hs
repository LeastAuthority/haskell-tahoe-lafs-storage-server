module MiscSpec
  ( spec
  ) where

import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  )

import FilesystemBackend
  ( partitionM
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

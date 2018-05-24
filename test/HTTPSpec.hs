{-# LANGUAGE OverloadedStrings #-}

module HTTPSpec
  ( spec
  ) where

import Data.ByteString
  ( ByteString
  )

import qualified Data.ByteString.Lazy as L

import Network.HTTP.Types.Method
  ( methodPost
  )

import Test.Hspec
  ( Spec
  , describe
  , it
  , shouldBe
  )

import Test.Hspec.Wai
  ( WaiSession
  , with
  , get
  , post
  , shouldRespondWith
  , request
  )
import Network.Wai.Test
  ( SResponse
  )

import Backend
  ( Backend
  )

import MemoryBackend
  ( memoryBackend
  )

import Server
  ( app
  )

postJSON :: ByteString -> L.ByteString -> WaiSession SResponse
postJSON path body =
  request methodPost path [("Content-Type", "application/json")] body

allocateBucketsJSON :: L.ByteString
allocateBucketsJSON =
  "{renew-secret: 'abcdefgh', cancel-secret: 'ijklmnop', share-numbers: [1, 3, 5], allocated-size: 512}"

spec :: Spec
spec = with (return $ app memoryBackend) $
  describe "v1" $ do
    describe "GET /v1/version" $ do
      it "responds with OK" $
        get "/v1/version" `shouldRespondWith` 200

    describe "POST /v1/immutable/abcdefgh" $ do
      it "responds with CREATED" $
        postJSON
          "/v1/immutable/abcdefgh"
          allocateBucketsJSON
          `shouldRespondWith` 201

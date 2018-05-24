{-# LANGUAGE OverloadedStrings #-}

module HTTPSpec
  ( spec
  ) where

import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map

import Data.Aeson.Types
  ( Value(String, Number, Array)
  )

import Data.Aeson
  ( encode
  )

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
  , ResponseMatcher(ResponseMatcher)
  , matchBody
  , matchStatus
  , matchHeaders
  , (<:>)
  )

import Test.Hspec.Wai.Matcher
  ( bodyEquals
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
  request
    methodPost
    path
    [("Content-Type", "application/json"), ("Accept", "application/json")]
    body

allocateBucketsJSON :: L.ByteString
allocateBucketsJSON =
  encode $ Map.fromList
  [ ("renew-secret" :: String, String "abcdefgh")
  , ("cancel-secret" :: String, String "ijklmnop")
  , ("share-numbers" :: String, Array (Vector.fromList [Number 1, Number 3, Number 5]))
  , ("allocated-size" :: String, Number 512)
  ]

allocateResultJSON :: L.ByteString
allocateResultJSON =
  encode $ Map.fromList
  [ ("already-have" :: String, Array Vector.empty)
  , ("allocated" :: String, Array (Vector.fromList [Number 1, Number 3, Number 5]))
  ]

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
          -- TODO: ;charset=utf-8 is just an artifact of Servant, would be
          -- nice to turn it off and not assert it here.
          { matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]
          , matchBody = bodyEquals allocateResultJSON
          }

{-# LANGUAGE OverloadedStrings #-}

module HTTPSpec (
    spec,
) where

import Prelude hiding (
    replicate,
 )

import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import GHC.Int (
    Int64,
 )

import Data.Aeson.Types (
    Value (Array, Number, String),
 )

import Data.Aeson (
    encode,
 )

import Data.ByteString (
    ByteString,
 )

import qualified Data.ByteString.Lazy as L

import Network.HTTP.Types.Method (
    methodGet,
    methodPost,
    methodPut,
 )

import Test.Hspec (
    Spec,
    describe,
    it,
 )

import Test.Hspec.Wai (
    WaiSession,
    matchBody,
    matchHeaders,
    request,
    shouldRespondWith,
    with,
    (<:>),
 )

import Test.Hspec.Wai.Matcher (
    bodyEquals,
 )

import Network.Wai.Test (
    SResponse,
 )

import TahoeLAFS.Storage.Backend.Null (
    NullBackend (NullBackend),
 )

import TahoeLAFS.Storage.Server (
    app,
 )

getJSON :: ByteString -> WaiSession st SResponse
getJSON path =
    request
        methodGet
        path
        [("Accept", "application/json")]
        ""

postJSON :: ByteString -> L.ByteString -> WaiSession st SResponse
postJSON path body =
    request
        methodPost
        path
        [("Content-Type", "application/json"), ("Accept", "application/json")]
        body

putShare :: ByteString -> Int64 -> WaiSession st SResponse
putShare path size =
    request
        methodPut
        path
        [("Content-Type", "application/octet-stream"), ("Accept", "application/json")]
        (L.replicate size 0xdd)

allocateBucketsJSON :: L.ByteString
allocateBucketsJSON =
    encode $
        Map.fromList
            [ ("renew-secret" :: String, String "abcdefgh")
            , ("cancel-secret" :: String, String "ijklmnop")
            , ("share-numbers" :: String, Array (Vector.fromList [Number 1, Number 3, Number 5]))
            , ("allocated-size" :: String, Number 512)
            ]

allocateResultJSON :: L.ByteString
allocateResultJSON =
    encode $
        Map.fromList
            [ ("already-have" :: String, Array Vector.empty)
            , ("allocated" :: String, Array Vector.empty)
            ]

corruptionJSON :: L.ByteString
corruptionJSON =
    encode $
        Map.fromList
            [ ("reason" :: String, "foo and bar" :: String)
            ]

sharesResultJSON :: L.ByteString
-- Simple enough I won't go through Aeson here
sharesResultJSON = "[]"

readResultJSON :: L.ByteString
-- Simple, again.
readResultJSON = "{}"

spec :: Spec
spec = with (return $ app NullBackend) $
    describe "v1" $ do
        describe "GET /v1/version" $ do
            it "responds with OK" $
                getJSON "/v1/version" `shouldRespondWith` 200

        describe "POST /v1/immutable/abcdefgh" $ do
            it "responds with CREATED" $
                postJSON
                    "/v1/immutable/abcdefgh"
                    allocateBucketsJSON
                    `shouldRespondWith` 201
                        { -- TODO: ;charset=utf-8 is just an artifact of Servant, would be
                          -- nice to turn it off and not assert it here.
                          matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]
                        , matchBody = bodyEquals allocateResultJSON
                        }

        describe "PUT /v1/immutable/abcdefgh/1" $ do
            it "responds with CREATED" $
                putShare "/v1/immutable/abcdefgh/1" 512 `shouldRespondWith` 201

        describe "POST /v1/immutable/abcdefgh/1/corrupt" $ do
            it "responds with OK" $
                postJSON
                    "/v1/immutable/abcdefgh/1/corrupt"
                    corruptionJSON
                    `shouldRespondWith` 200

        describe "GET /v1/immutable/abcdefgh/shares" $ do
            it "responds with OK and a JSON list" $
                getJSON "/v1/immutable/abcdefgh/shares"
                    `shouldRespondWith` 200
                        { matchBody = bodyEquals sharesResultJSON
                        }

        describe "GET /v1/immutable/abcdefgh" $ do
            it "responds with OK and a JSON object" $
                getJSON "/v1/immutable/abcdefgh"
                    `shouldRespondWith` 200
                        { matchBody = bodyEquals readResultJSON
                        }

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ServantUtil
  ( CBOR
  ) where

import Network.HTTP.Media
  ( (//)
  )

import Data.ByteString
  ( ByteString
  )

import Data.ByteString.Base64
  ( encode
  , decode
  )

import Data.Text.Encoding
  ( decodeUtf8
  , encodeUtf8
  )

import Data.ByteString.UTF8
  ( toString
  , fromString
  )

import Servant
  ( Accept(..)
  , MimeRender(..)
  , MimeUnrender(..)
  )

import Data.Aeson
  ( ToJSON(toJSON)
  , FromJSON(parseJSON)
  , withText
  )
import Data.Aeson.Types
  ( Parser
  , Result(Error, Success)
  , fromJSON
  , Value(String)
  )

import Codec.CBOR.Write
  ( toLazyByteString
  )

import Codec.CBOR.Read
  ( deserialiseFromBytes
  )

import Codec.CBOR.JSON
  ( encodeValue
  , decodeValue
  )

data CBOR

instance Accept CBOR where
  -- https://tools.ietf.org/html/rfc7049#section-7.3
  contentType _ = "application" // "cbor"

instance ToJSON a => MimeRender CBOR a where
  mimeRender _ val = toLazyByteString $ encodeValue $ toJSON val

instance FromJSON a => MimeUnrender CBOR a where
  mimeUnrender _ bytes =
    case deserialiseFromBytes (decodeValue False) bytes of
      Right ("", val)  ->
        case fromJSON val of
          Error s   -> Left s
          Success x -> Right x
      Right (extra, _) -> Left "extra bytes at tail"
      Left err         -> Left $ show err


instance ToJSON ByteString where
  toJSON bs = String $ decodeUtf8 $ encode bs

instance FromJSON ByteString where
  parseJSON = withText "String" (
    \bs ->
      case decode $ encodeUtf8 bs of
        Left err    -> fail ("Base64 decoding failed: " ++ err)
        Right bytes -> return bytes
    )

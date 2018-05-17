{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ServantUtil
  ( CBOR
  ) where

import Network.HTTP.Media
  ( (//)
  )

import Servant
  ( Accept(..)
  , MimeRender(..)
  , MimeUnrender(..)
  )

import Data.Aeson
  ( ToJSON(toJSON)
  , FromJSON
  )
import Data.Aeson.Types
  ( Parser
  , Result(Error, Success)
  , fromJSON
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module TahoeLAFS.Storage.APIDocs where

import Servant
  ( Capture(..)
  )

import Servant.Docs
  ( ToCapture(toCapture)
  , ToSample
  , DocCapture(DocCapture)
  )

import TahoeLAFS.Storage.API
  ( StorageIndex
  )

instance ToCapture (Capture "storage_index" StorageIndex) where
  toCapture _ = DocCapture "storage index" "(hex string) a storage index to use to address the data"

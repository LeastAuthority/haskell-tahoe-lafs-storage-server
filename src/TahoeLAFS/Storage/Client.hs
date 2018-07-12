module TahoeLAFS.Storage.Client
  ( version
  , createImmutableStorageIndex
  , writeImmutableShare
  , adviseCorruptImmutableShare
  , getImmutableShareNumbers
  , readImmutableShares
  , createMutableStorageIndex
  , readvAndTestvAndWritev
  , readMutableShares
  , getMutableShareNumbers
  , adviseCorruptMutableShare
  ) where

import Servant
  ( (:<|>)(..)
  )
import Servant.Client
  ( client
  )

import TahoeLAFS.Storage.API
  ( api
  )

(version
  :<|> createImmutableStorageIndex
  :<|> writeImmutableShare
  :<|> adviseCorruptImmutableShare
  :<|> getImmutableShareNumbers
  :<|> readImmutableShares
  :<|> createMutableStorageIndex
  :<|> readvAndTestvAndWritev
  :<|> readMutableShares
  :<|> getMutableShareNumbers
  :<|> adviseCorruptMutableShare
  ) = client api

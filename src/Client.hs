module Client
  (
    version
  , allocateBuckets
  , getBuckets
  , writeToBucket
  , readFromBucket
  , adviseCorruptBucket
  , testvAndReadvAndWritev
  , readv
  ) where

import Servant
  ( (:<|>)(..)
  )
import Servant.Client
  ( client
  )

import Storage
  ( api
  )

(version
 :<|> allocateBuckets
 :<|> getBuckets
 :<|> writeToBucket
 :<|> readFromBucket
 :<|> adviseCorruptBucket
 :<|> testvAndReadvAndWritev
 :<|> readv
  ) = client api

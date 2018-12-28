module Main where

import System.FilePath
  ( FilePath
  , (</>)
  )

import Servant.PY
  ( writePythonForAPI
  , requests
  )

import TahoeLAFS.Storage.API
  ( api
  )

result :: FilePath
result = "./"

main :: IO ()
main = writePythonForAPI api requests (result </> "requests_api.py")

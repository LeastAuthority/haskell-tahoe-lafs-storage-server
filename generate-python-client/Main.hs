module Main where

import System.FilePath
  ( FilePath
  , (</>)
  )

import Servant.PY
  ( writePythonForAPI
  , requests
  , treq
  )

import TahoeLAFS.Storage.API
  ( api
  )

result :: FilePath
result = "./"

main :: IO ()
main = do
    writePythonForAPI api requests (result </> "requests_api.py")
    writePythonForAPI api treq (result </> "treq_api.py")

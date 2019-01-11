module Main where

import System.FilePath
  ( FilePath
  , (</>)
  )

import Servant.JS
  ( writeJSForAPI
  , angular
  , defAngularOptions
  , vanillaJS
  , jquery
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

    writeJSForAPI api (angular defAngularOptions) (result </> "angular_api.js")
    writeJSForAPI api vanillaJS (result </> "vanilla_api.js")
    writeJSForAPI api jquery (result </> "jquery_api.js")

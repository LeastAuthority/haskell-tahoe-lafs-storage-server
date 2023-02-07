module Main where

import Prelude hiding (
    putStrLn,
 )

import Data.Text (
    Text,
    pack,
    unpack,
 )

import Data.Text.IO (
    putStrLn,
 )

import Text.Pandoc.Class (
    PandocIO,
    runIOorExplode,
 )

import Text.Pandoc.Writers.HTML (
    writeHtml5String,
 )

import Text.Blaze.Html.Renderer.Pretty (
    renderHtml,
 )

import Servant.Docs (
    docs,
 )

-- import Servant.Docs.Pandoc
--   ( pandoc
--   )

import Data.Default (
    def,
 )

-- Import for instances only.
import TahoeLAFS.Storage.APIDocs (

 )

import TahoeLAFS.Storage.API (
    api,
 )

main :: IO ()
main = do
    let doc = pandoc $ docs $ api
    let markup = writeHtml5String def doc
    html <- runIOorExplode markup
    putStrLn html

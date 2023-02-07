module Main where

import Test.Hspec.Runner (
    hspec,
 )

import Spec (
    spec,
 )

main :: IO ()
main = do
    hspec spec

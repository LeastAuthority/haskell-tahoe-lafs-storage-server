module Spec where

import Test.Hspec

import qualified HTTPSpec as H
import qualified MiscSpec as M
import qualified SemanticSpec as S

spec :: Spec
spec = do
    parallel $ describe "HTTP" H.spec
    parallel $ describe "Misc" M.spec
    parallel $ describe "Semantic" S.spec

module Main where

import qualified EvalSpec
import qualified ParserSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "ParsecSpec" $ ParserSpec.specs
  describe "EvalSpec" $ EvalSpec.specs

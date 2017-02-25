module Main where

import qualified EvalSpec
import qualified ParserSpec
import qualified Parser.InternalSpec as ParserInternalSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "ParsecSpec" $ do
    ParserInternalSpec.specs
    ParserSpec.specs
  describe "EvalSpec" $ EvalSpec.specs

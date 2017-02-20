module Main where

import Test.Hspec
import qualified ParserSpec


main :: IO ()
main = hspec $ ParserSpec.specs

-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"


module Main where

import System.Environment (getArgs)
import Lib
import Parser (readExpr)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)

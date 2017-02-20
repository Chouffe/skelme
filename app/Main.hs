module Main where

import System.Environment (getArgs)
import Parser (readExpr)

main :: IO ()
main = do
  (expr:_) <- getArgs
  print (readExpr expr)

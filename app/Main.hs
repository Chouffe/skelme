module Main where

import           Eval               (runOne)
import           Repl               (runRepl)
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \args ->
  if null args then runRepl else runOne $ args

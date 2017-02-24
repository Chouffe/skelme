module Main where

import           Eval               (runOne)
import           Repl               (runReplSession)
import           System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \args ->
  if null args
  then runReplSession
  else runOne $ args

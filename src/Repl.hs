module Repl where

import System.IO

import Control.Monad
import Eval (eval)
import Parser (readExpr')
import Data
  ( Env
  , liftThrows
  , runIOThrows
  , emptyEnv
  )

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString envRef expr =
  runIOThrows $ liftM show $ (liftThrows $ readExpr' expr) >>= eval envRef

evalAndPrint :: Env -> String -> IO ()
evalAndPrint envRef str = evalString envRef str >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ p prompt action = do
  result <- prompt
  if p result
  then return ()
  else action result >> until_ p prompt action

runRepl :: IO ()
runRepl = emptyEnv >>= (\env -> until_ (== "quit") (readPrompt "λ> ") (evalAndPrint env))

module Repl where

import System.IO

import Data.IORef (readIORef)
import Control.Monad
import Eval (eval, primitiveBindings)
import Parser (readExpr')
import Data
  ( Env
  , liftThrows
  , runIOThrows
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
runRepl =
  primitiveBindings >>=
    (\env -> until_ (== "quit")
                    (readPrompt "λ> ")
                    (evalAndPrint env))

-- Prints the environment after each evaluation
runDebugRepl :: IO ()
runDebugRepl =
  primitiveBindings >>=
    (\env -> until_ (== "quit") (readPrompt "λ> ")
      (\s -> do result <- evalAndPrint env s
                printEnv env
                return result))

-- Debugging purpose
printEnv :: Env -> IO ()
printEnv envRef = do
  env <- readIORef envRef
  mapM_ (\(var, ioVal) -> readIORef ioVal >>= \val -> putStrLn $ var ++ "\t -> \t" ++ show val) env

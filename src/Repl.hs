module Repl where

import System.IO

import Eval (eval)
import Parser (readExpr')

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString str = do
  case readExpr' str of
    Left err -> return $ show err
    Right lispval -> return $
      case eval lispval of
        Left err -> show err
        Right val -> show val

evalAndPrint :: String -> IO ()
evalAndPrint str = evalString str >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ p prompt action = do
  result <- prompt
  if p result
  then return ()
  else action result >> until_ p prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "λ> ") evalAndPrint

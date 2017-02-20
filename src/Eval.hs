module Eval where

import           Control.Monad.Except
import           Data

eval :: LispVal -> ThrowsError LispVal
eval val@(Bool _)               = return val
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args))  = mapM eval args >>= apply func
-- TODO
eval badForm                    = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NoFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinOp (+))
  , ("-", numericBinOp (-))
  , ("*", numericBinOp (*))
  , ("/", numericBinOp div)
  , ("%", numericBinOp mod)
  ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _ [] =  throwError $ NumArgs 2 []
numericBinOp _ val@[_] =  throwError $ NumArgs 2 val
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

-- TODO: use GADTs or Phantom Types to make this impossible at compile time
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = throwError $ TypeMismatch "number" $ String s
unpackNum badForm = throwError $ TypeMismatch "number" badForm

module Eval where

import           Data

eval :: LispVal -> LispVal
eval val@(Bool _)               = val
eval val@(String _)             = val
eval val@(Number _)             = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args
-- TODO
eval _                          = undefined

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinOp (+))
  , ("-", numericBinOp (-))
  , ("*", numericBinOp (*))
  , ("/", numericBinOp div)
  , ("%", numericBinOp mod)
  ]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op params = Number $ foldl1 op $ map unpackNum params

-- TODO: use GADTs or Phantom Types to make this impossible at compile time
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

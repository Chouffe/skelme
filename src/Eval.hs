module Eval where

import           Control.Monad.Except
import           Data

eval :: LispVal -> ThrowsError LispVal
-- Primitives
eval val@(Bool _)               = return val
eval val@(String _)             = return val
eval val@(Number _)             = return val

-- Special Forms
eval (List [Atom "quote", val]) = return val
eval (List (Atom "car" : val))  = car val
eval (List (Atom "cdr" : val))  = cdr val
eval (List (Atom "cons" : val)) = cons val
eval (List (Atom "eqv" : val))  = eqv val

eval (List [Atom "if", predicate, conseq, alt]) = do
  result <- eval predicate
  case result of
    Bool False -> eval alt
    _          -> eval conseq

-- Function application
eval (List (Atom func : args))  = mapM eval args >>= apply func

-- BadSpecialForm
eval badForm                    = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Special forms
car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]         = return x
car [DottedList (x : _) _] = return x
car [badArg]               = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs]         = return $ List $ x:xs
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x1, x2]             = return $ DottedList [x1] x2
cons badArgList           = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool b1), (Bool b2)] = return $ Bool $ b1 == b2
eqv [(Number n1), (Number n2)] = return $ Bool $ n1 == n2
eqv [(String s1), (String s2)] = return $ Bool $ s1 == s2
eqv [(Atom a1), (Atom a2)] = return $ Bool $ a1 == a2
eqv [(DottedList xs1 x1), (DottedList xs2 x2)] = eqv [(List $ x1 : xs1), (List  $ x2 : xs2)]
eqv [(List xs), (List ys)] =
  return $ Bool $ all eqvPair $ zip xs ys
    where eqvPair (x1, x2) =
            case eqv [x1, x2] of
              Left _             -> False
              (Right (Bool val)) -> val
              _                  -> False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NoFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinOp (+))
  , ("-", numericBinOp (-))
  , ("*", numericBinOp (*))
  , ("/", numericBinOp div)
  , ("%", numericBinOp mod)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  ]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
  then throwError $ NumArgs 2 args
  else do left <- unpacker $ args !! 0
          right <- unpacker $ args !! 1
          return $ Bool $ left `op` right

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop  = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop  = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp _ []      =  throwError $ NumArgs 2 []
numericBinOp _ val@[_] =  throwError $ NumArgs 2 val
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

-- TODO: use GADTs or Phantom Types to make this impossible at compile time
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = throwError $ TypeMismatch "number" $ String s
unpackNum badForm    = throwError $ TypeMismatch "number" badForm

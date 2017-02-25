module Parser
  ( parseExpr
  , readExpr
  , readExprList
  )
  where

import           Control.Monad.Except
import           Text.ParserCombinators.Parsec (endBy, parse, Parser)

import           Data
import Parser.Internal (parseExpr, spaces)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
  case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

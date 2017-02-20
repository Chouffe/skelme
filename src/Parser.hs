module Parser where

import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

import           Control.Applicative           hiding (many, (<|>))
import           Control.Monad

import           Data

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = String <$> between (char '"') (char '"') (many (noneOf "\""))

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom


parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> between (char '(') (char ')') (sepBy parseExpr spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
  (h, t) <- between (char '(') (char ')') inDottedList
  return $ DottedList h t
  where inDottedList = do
          head <- endBy parseExpr spaces
          tail <- char '.' >> spaces >> parseExpr
          return $ (head, tail)

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> (try parseList <|> parseDottedList)

readExpr :: String -> String
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Value found"

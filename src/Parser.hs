module Parser where

import           Control.Monad.Except
import           Text.ParserCombinators.Parsec hiding (spaces)

import           Data

-- TODO: move to Impl and export only public API here which is parseExpr

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


parseNegativeInt :: Parser Integer
parseNegativeInt = char '-' *> ((flip subtract 0) <$> parsePositiveInt)

parsePositiveInt :: Parser Integer
parsePositiveInt = read <$> many1 digit

parseNumber :: Parser LispVal
parseNumber = Number <$> (parseNegativeInt <|> parsePositiveInt)

parseList :: Parser LispVal
parseList = List <$> between (char '(') (char ')') (sepBy parseExpr spaces)

parseDottedList :: Parser LispVal
parseDottedList = do
  (h, t) <- between (char '(') (char ')') inDottedList
  return $ DottedList h t
  where inDottedList = do
          h <- endBy parseExpr spaces
          t <- char '.' >> spaces >> parseExpr
          return $ (h, t)

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- TODO: fix negative numbers that get parsed by parseAtom (should be in a try to backtrack
parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseNumber
          <|> parseString
          <|> (try parseList <|> parseDottedList)

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "lisp" input of
    Left err  -> String $ "No match: " ++ show err
    Right val -> val

readExpr' :: String -> ThrowsError LispVal
readExpr' input =
  case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

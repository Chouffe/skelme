module Parser.Internal
  ( spaces
  , symbol
  , parseString
  , parseAtom
  , parseNumber
  , parseList
  , parseDottedList
  , parseQuoted
  , parseExpr
  )
  where

import           Text.Parsec.Char              (string)
import           Text.ParserCombinators.Parsec hiding (spaces)

import           Data

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

minusAtom :: Parser LispVal
minusAtom = Atom <$> string "-"

minusPrefix :: Parser LispVal
minusPrefix = do
  minus <- minusAtom
  expr <- parseExpr
  return $ List [minus, expr]

symbolFirstLetter :: Parser Char
symbolFirstLetter = oneOf "!#$%&|*+/:<=>?@^_~"

parseString :: Parser LispVal
parseString = String <$> between (char '"') (char '"') (many (noneOf "\""))

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbolFirstLetter
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

parsePositiveInt :: Parser Integer
parsePositiveInt = read <$> many1 digit

parseNegativeInt :: Parser Integer
parseNegativeInt = char '-' *> ((flip subtract 0) <$> parsePositiveInt)

parseNumber :: Parser LispVal
parseNumber = ((parseNegativeInt >>= (\n -> return $ List [Atom "-", Number (-n)])) <|> (Number <$> parsePositiveInt))

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

parseExpr :: Parser LispVal
parseExpr =
  (try minusPrefix <|> minusAtom <|> parseAtom  <|> parseNumber)
   <|> parseString
   <|> parseQuoted
   <|> (try parseList <|> parseDottedList)

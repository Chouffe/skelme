module Data where

import           Control.Monad.Except
import           Data.List           (intercalate)
import           Text.Parsec.Error   (ParseError)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
  deriving (Eq)

unwordsList :: Show a => [a] -> String
unwordsList = intercalate " " . map show

instance Show LispVal where
  show (Atom name) = name
  show (List xs) = "(" ++ unwordsList xs ++ ")"
  show (DottedList hs t) = "(" ++ (unwordsList hs) ++ " . " ++ show t ++ ")"
  show (Number k) = show k
  show (String s) = "\"" ++ s ++ "\""
  show (Bool b) = case b of
                    True  -> "#t"
                    False -> "#f"

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NoFunction String String
               | UnboundVar String String
               | Default String
  deriving (Eq)

instance Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid Type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NoFunction message func) = message ++ ": " ++ show func
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (Default s) = "Error: " ++ s

type ThrowsError = Either LispError

-- Polymorphic enough?
trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

-- TODO: avoid
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue _ = error "ERROR"

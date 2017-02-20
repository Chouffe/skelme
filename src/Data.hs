module Data where

import Data.Maybe (isJust)
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.IORef
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

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwE err
liftThrows (Right val) = return val

-- Environment

type Env = IORef [(String, IORef LispVal)]

emptyEnv :: IO Env
emptyEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  return $ isJust (lookup var env)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwE $ UnboundVar "Getting an unbound var" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar = undefined

definedVar :: Env -> String -> LispVal -> IOThrowsError LispVal
definedVar = undefined



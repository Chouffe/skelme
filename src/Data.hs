{-# LANGUAGE TupleSections #-}

module Data where

import           Control.Monad              (liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.IORef
import           Data.List                  (intercalate)
import           Data.Maybe                 (isJust)
import           Text.Parsec.Error          (ParseError)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params  :: [String]
                    , vararg  :: (Maybe String)
                    , body    :: [LispVal]
                    , closure :: Env }

-- TODO: add an equal instance for LispVal

makeFunc :: (Maybe String) -> Env -> [String] -> [LispVal] -> IOThrowsError LispVal
makeFunc v env p b = return $ Func p v b env

makeNormalFunc :: Env -> [String] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: String -> Env -> [String] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . show

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

  show (PrimitiveFunc _) = "<primitive>"
  show (Func args _ b _) = "(lambda  (" ++ unwordsList args ++  ") (" ++ unwordsList b ++ "))"

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NoFunction String String
               | UnboundVar String String
               | Default String

-- TODO: add an equal instance for LispError

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

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do
  e <- runExceptT action
  case e of
    Right val -> return $ val
    Left err  -> return $ show err

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwE err
liftThrows (Right val) = return val

-- Environment

type Env = IORef [(String, IORef LispVal)]

-- Private API

isBound :: Env -> String -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  return $ isJust (lookup var env)

-- Public API

emptyEnv :: IO Env
emptyEnv = newIORef []

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwE $ UnboundVar "Getting an unbound var" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwE $ UnboundVar "Setting an unbound var" var)
        (liftIO . (flip writeIORef val))
        (lookup var env)
  return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
  alreadyBound <- liftIO $ isBound envRef var
  if alreadyBound
  then setVar envRef var val
  else do
    valRef <- liftIO $ newIORef val
    liftIO $ modifyIORef envRef ((var, valRef):)
    return $ val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
  readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bs env = liftM (++ env) $ mapM addBinding bs
        addBinding (var, value) = (var,) <$> newIORef value

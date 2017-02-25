module Repl where

import           System.IO

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.State
import           Data                       (Env, LispVal, ThrowsError,
                                             liftThrows, liftThrows,
                                             runIOThrows)
import           Data.Either                (either)
import           Data.IORef                 (readIORef)
import           Data.List                  (intercalate)
import           Eval                       (eval, primitiveBindings)
import           Parser                     (readExpr)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString envRef expr =
  runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval envRef

evalAndPrint :: Env -> String -> IO ()
evalAndPrint envRef str = evalString envRef str >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ p prompt action = do
  result <- prompt
  if p result
  then return ()
  else action result >> until_ p prompt action

runRepl :: IO ()
runRepl =
  primitiveBindings >>=
    (\env -> until_ (== "quit")
                    (readPrompt "λ> ")
                    (evalAndPrint env))

-- Prints the environment after each evaluation
runDebugRepl :: IO ()
runDebugRepl =
  primitiveBindings >>=
    (\env -> until_ (== "quit") (readPrompt "λ> ")
      (\s -> do result <- evalAndPrint env s
                printEnv env
                return result))

-- Debugging purpose
printEnv :: Env -> IO ()
printEnv envRef = do
  env <- readIORef envRef
  mapM_ (\(var, ioVal) -> readIORef ioVal >>= \val -> putStrLn $ var ++ "\t -> \t" ++ show val) env

-- Repl Session

type SessionVal = ThrowsError LispVal
newtype Session = Session [(String, SessionVal)]
type ReplSession = StateT Session IO ()

instance Show Session where
  show (Session xs) = delimiter ++ intercalate "\n" rows ++ delimiter
    where rows = map showSessionRow xs
          delimiter = "\n---------------------------------\n"
          showSessionRow (input, Right lispVal) = input ++ " :λ>  " ++ show lispVal
          showSessionRow (input, Left err) = input ++ " :λ> error: " ++ show err

consToSession :: String -> SessionVal -> Session -> Session
consToSession input val (Session session) = Session $ (input, val) : session

replSession :: Env -> ReplSession
replSession env = do
  input <- liftIO $ (readPrompt "λ> ")
  if (input == ":h")
  then do
    session <- get
    liftIO $ replHook input session
    replSession env
  else
    case readExpr input of
      errorVal@(Left err) -> do
        modify (consToSession input errorVal)
        liftIO $ print err
        replSession env
      Right parsedVal -> do
        evaled <- liftIO $ runExceptT $ eval env parsedVal
        modify (consToSession input evaled)
        liftIO $ putStrLn $ either show show evaled
        replSession env

runReplSession :: IO ()
runReplSession = do
  env <- primitiveBindings
  _ <- runStateT (replSession env) $ Session []
  return ()

replHook :: String -> Session -> IO ()
replHook input session = do
  case input of
    ":q" -> fail "TODO: quit the repl"
    ":h" -> print session
    _    -> return ()

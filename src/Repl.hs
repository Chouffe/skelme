module Repl (runRepl) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.State
import           Data                       (Env, LispVal, ThrowsError,
                                             getVarNames)
import           Data.Either                (either)
import           Data.List                  (intercalate, sort)
import           Eval                       (eval, load, primitiveBindings)
import           Parser                     (readExpr)
import           System.Console.Haskeline

type SessionVal = ThrowsError LispVal
newtype Session = Session [(String, SessionVal)]
type ReplSession = StateT Session (InputT IO) ()

-- TODO: use Text.PrettyPrint instead of janky tabs
instance Show Session where
  show (Session xs) = delimiter ++ intercalate "\n" rows ++ delimiter
    where n = length xs
          rows = zipWith showSessionRow (reverse xs) (reverse (take n [0..] :: [Integer]))
          delimiter = "\n---------------------------------\n"
          showSessionRow (input, Right lispVal) idx = show idx ++ ":\t" ++ input ++ "\tλ: " ++ show lispVal
          showSessionRow (input, Left err) idx = show idx ++ ":\t" ++ input ++ "\tλ: error: " ++ show err

consToSession :: String -> SessionVal -> Session -> Session
consToSession input val (Session session) = Session $ (input, val) : session

handleReplInput :: Env -> String -> ReplSession
handleReplInput env input =
  case readExpr input of
    val@(Left err) -> do
      modify (consToSession input val)
      liftIO (print err)
      replSession env
    Right parsedVal -> do
      evaled <- liftIO $ runExceptT $ eval env parsedVal
      modify (consToSession input evaled)
      liftIO $ putStrLn $ either show show evaled
      replSession env

reloadRepl :: ReplSession
reloadRepl = do
  newEnv <- liftIO $ primitiveBindings
  liftIO $ loadCoreSkelmeLibrary newEnv
  replSession newEnv

replSession :: Env -> ReplSession
replSession env = do
  minput <- lift $ getInputLine "λ: "
  case minput of
    Nothing    -> return ()
    Just ":q"  -> return ()
    Just ":r"  -> liftIO (putStrLn "Reloading repl...") >> reloadRepl
    Just ":s"  -> get >>= liftIO . print >> replSession env
    Just ":h"  -> liftIO (putStrLn replHelp) >> replSession env
    Just ":b"  -> liftIO (getVarNames env >>= printVarNames) >> replSession env
    Just input -> handleReplInput env input

printVarNames :: [String] -> IO ()
printVarNames = mapM_ putStrLn . sort

loadCoreSkelmeLibrary :: Env -> IO ()
loadCoreSkelmeLibrary env =
  (runExceptT $ load "lib/core.sklm" >>= liftM last . mapM (eval env)) >> return ()

replVersion :: String
replVersion = "skelme v0.1"

replHelp :: String
replHelp =
  unlines [ ":b to browse bound vars"
          , ":h to show this help menu"
          , ":q to quit"
          , ":r to reload"
          , ":s to show session history"
          ]

-- TODO: add autocomplete
readlineSettings :: MonadIO m => Settings m
readlineSettings = defaultSettings

runRepl :: IO ()
runRepl = do
  skelmeLogo <- readFile "lib/logo.txt"
  putStrLn skelmeLogo
  putStrLn replVersion
  putStrLn replHelp
  env <- primitiveBindings
  loadCoreSkelmeLibrary env
  _ <- runInputT readlineSettings $ runStateT (replSession env) $ Session []
  return ()

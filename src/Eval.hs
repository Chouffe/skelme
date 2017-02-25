module Eval where

import           Control.Monad.Except
import           Data
import           Parser               (readExpr, readExprList)
import           System.IO            (IOMode (ReadMode, WriteMode), hClose,
                                       hGetLine, hPrint, hPutStrLn, openFile,
                                       readFile, stderr, stdin, stdout)

eval :: Env -> LispVal -> IOThrowsError LispVal
-- Primitives
eval _ val@(Bool _)                       = return val
eval _ val@(String _)                     = return val
eval _ val@(Number _)                     = return val
eval _ val@(List [Atom "quote", List []]) = return val  -- Nil Value

-- Atoms
eval env (Atom s)                 = getVar env s

-- Special Forms
eval _ (List [Atom "quote", val]) = return val

eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var

eval env (List (Atom "define" : (List (Atom var : p)) : b)) =
  makeNormalFunc env (map show p) b >>= defineVar env var

-- TODO: function definition for DottedList
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var

eval env (List [Atom "if", predicate, conseq, alt]) = do
  result <- eval env predicate
  case result of
    Bool False -> eval env alt
    _          -> eval env conseq

eval env (List (Atom "lambda" : List p : b)) = makeNormalFunc env (map show p) b

eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)

-- TODO: function application for dottedList
-- Function application
eval env (List (function : args)) = do
  lambda  <- eval env function
  argVals <- mapM (eval env) args
  apply lambda argVals

-- BadSpecialForm
eval _ badForm                    = throwError $ BadSpecialForm "Unrecognized special form" badForm

car :: [LispVal] -> ThrowsError LispVal
car [List [Atom "quote", List []]] = return nil
car [List (x : _)]                 = return x
car [DottedList (x : _) _]         = return x
car [badArg]                       = throwError $ TypeMismatch "pair" badArg
car badArgList                     = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List [Atom "quote", List []]] = return $ List [x]
cons [x, List xs]                      = return $ List $ x:xs
cons [x, DottedList xs y]              = return $ DottedList (x:xs) y
cons [x1, x2]                          = return $ DottedList [x1] x2
cons badArgList                        = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool b1), (Bool b2)] = return $ Bool $ b1 == b2
eqv [(Number n1), (Number n2)] = return $ Bool $ n1 == n2
eqv [(String s1), (String s2)] = return $ Bool $ s1 == s2
eqv [(Atom a1), (Atom a2)] = return $ Bool $ a1 == a2
eqv [(DottedList xs1 x1), (DottedList xs2 x2)] = eqv [(List $ x1 : xs1), (List  $ x2 : xs2)]
eqv [List [], List []] = return $ Bool True
eqv [(List xs), (List ys)] =
  return $ Bool $ all eqvPair $ zip xs ys
    where eqvPair (x1, x2) =
            case eqv [x1, x2] of
              Left _             -> False
              (Right (Bool val)) -> val
              _                  -> False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func p v b c) args =
  if length p /= length args && v == Nothing
  then throwError $ NumArgs (num p) args
  else (liftIO $ bindVars c $ zip p args) >>= evalBody
  where num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) b
apply (IOFunc func) args = func args
apply val args = liftThrows $ throwError $ NoFunction ("trying to call an unboud function with args" ++ (show val) ++ (show args)) (show val)

ioPrimitives :: [(String, ([LispVal] -> IOThrowsError LispVal))]
ioPrimitives =
  [ ("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  ]

-- TODO: Should it be an ioPrimitive???
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, (List args)] = apply func args
applyProc (func : args) = apply func args
applyProc _ = liftThrows $ throwError $ Default "bad arguments passed to apply"

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ _ = liftThrows $ throwError $ Default "expecting a filename"

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = liftThrows $ throwError $ Default "expecting a port"

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc _           = liftThrows $ throwError $ Default "expecting a port"


writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc _ = liftThrows $ throwError $ Default "expecting a port"


readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents _ = liftThrows $ throwError $ Default "expecting a filename"

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll _ = liftThrows $ throwError $ Default "expecting a filename"

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eqv?", eqv)
  , ("+", numericBinOp (+))
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

primitiveBindings :: IO Env
primitiveBindings = emptyEnv >>= (flip bindVars $ map (makeFunc' IOFunc) ioPrimitives ++ map (makeFunc' PrimitiveFunc) primitives)
  where makeFunc' constructor (var, func) = (var, constructor func)

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
numericBinOp op parameters = mapM unpackNum parameters >>= return . Number . foldl1 op

-- TODO: use GADTs or Phantom Types to make this impossible at compile time
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = throwError $ TypeMismatch "number" $ String s
unpackNum badForm    = throwError $ TypeMismatch "number" badForm

-- TODO: move somewhere else
runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

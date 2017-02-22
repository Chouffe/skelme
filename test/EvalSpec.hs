module EvalSpec where

import           Test.Hspec
import           Test.QuickCheck            (Property, property, (==>))
import           Test.QuickCheck.Monadic    (assert, monadicIO, run)

import           Control.Monad.Trans.Except
import           Data
import           Eval                       (eval)
import           SpecUtils

-- Resource: Modadic QuickCheck - https://gist.github.com/ijt/967505

evalLispValToItself :: LispVal -> Property
evalLispValToItself lispVal = monadicIO $ do
  env <- run emptyEnv
  val <- run $ runExceptT $ eval env lispVal
  assert $ val == Right lispVal

specs :: Spec
specs =
  describe "Eval" $ do
    describe "primitives" $ do
      it "evals a string to itself" $ do
        env <- emptyEnv
        (runExceptT $ eval env (String "Hello")) `shouldReturn` (Right (String "Hello"))
      it "evals a string to itself" $
        property $ \s -> evalLispValToItself (String s)
      it "evals a bool to itself" $
        property $ \b -> evalLispValToItself (Bool b)
      it "evals a number to itself" $
        property $ \n -> evalLispValToItself (Number n)
      it "evals a quote to itself (positive number)" $
        property $ \n ->
          (n > 0) ==> monadicIO $ do
          env <- run $ emptyEnv
          val <- run $ runExceptT $ eval env (List [Atom "quote", Number n])
          assert $ val == Right (Number n)
      it "evals a quote to itself (negative number)" $
        property $ \n ->
          (n < 0) ==> monadicIO $ do
          env <- run $ emptyEnv
          val <- run $ runExceptT $ eval env (List [Atom "quote", Number n])
          assert $ val == Right (Number n)
      it "evals a quoted expr to an expr" $
        property $ \(ALispVal lispVal) ->
          monadicIO $ do
          env <- run $ emptyEnv
          val <- run $ runExceptT $ eval env (List [Atom "quote", lispVal])
          assert $ val == Right lispVal

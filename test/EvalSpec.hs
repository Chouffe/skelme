module EvalSpec where

import           Test.Hspec
import           Test.QuickCheck            (Property, property, (==>))
import           Test.QuickCheck.Monadic    (assert, monadicIO, run)

import           Control.Monad.Trans.Except
import           Data
import           Data.Either (isLeft)
import           Eval                       (eval, primitiveBindings)
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
      it "evals nil to itself" $
        evalLispValToItself nil
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

    describe "function application" $ do

      describe "simple arithmetic" $ do
        it "addition" $
          property $ \m -> \n -> monadicIO $ do
            env <- run $ primitiveBindings
            val <- run $ runExceptT $ eval env (List [Atom "+", Number m, Number n])
            assert $ val == Right (Number (n + m))
        it "subtraction" $
          property $ \m -> \n -> monadicIO $ do
            env <- run $ primitiveBindings
            val <- run $ runExceptT $ eval env (List [Atom "-", Number m, Number n])
            assert $ val == Right (Number (m - n))
        it "multiplication" $
          property $ \m -> \n -> monadicIO $ do
            env <- run $ primitiveBindings
            val <- run $ runExceptT $ eval env (List [Atom "*", Number m, Number n])
            assert $ val == Right (Number (m * n))

      describe "bad function application" $ do
        it "Number is not a symbol" $
          property $ \n (ALispVal expr) -> monadicIO $ do
            env <- run $ emptyEnv
            val <- run $ runExceptT $ eval env (List [Number n, expr])
            assert $ isLeft val == True
        it "Bool is not a symbol" $
          property $ \b (ALispVal expr) -> monadicIO $ do
            env <- run $ emptyEnv
            val <- run $ runExceptT $ eval env (List [Bool b, expr])
            assert $ isLeft val == True
        it "String is not a symbol" $
          property $ \s (ALispVal expr) -> monadicIO $ do
            env <- run $ emptyEnv
            val <- run $ runExceptT $ eval env (List [String s, expr])
            assert $ isLeft val == True

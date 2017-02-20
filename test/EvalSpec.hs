module EvalSpec where

import Test.QuickCheck (property)
import Test.Hspec

import Data
import Eval (eval)

specs :: Spec
specs =
  describe "Eval" $ do
    describe "primitives" $ do
      it "evals a string to itself" $ property $
        \s -> eval (String s) == Right (String s)
      it "evals a Bool to itself" $ property $
        \b -> eval (Bool b) == Right (Bool b)
      it "evals a Number to itself" $ property $
        \n -> eval (Number n) == Right (Number n)
      it "evals a quoted number to a number" $ property $
        \n -> eval (List [Atom "quote", (Number n)]) == Right (Number n)
      it "evals a quoted expr to an expr" $
        pendingWith "Make LispVal an instance of Arbitrary"

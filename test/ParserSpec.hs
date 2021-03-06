module ParserSpec where

import           Test.Hspec
import           Test.QuickCheck               (property, (==>))
import           Text.ParserCombinators.Parsec (parse)

import           Data
import           Parser                        (parseExpr)
import           SpecUtils

-- TODO: add extra tests for dotted lists

sourceName :: String
sourceName = "ParserSpec"

specs :: Spec
specs =
  describe "Parser" $ do

    describe "Arithmetic Atoms" $ do
      it "parses +" $ (parse parseExpr sourceName "+") == Right (Atom "+")
      it "parses -" $ (parse parseExpr sourceName "-") == Right (Atom "-")
      it "parses *" $ (parse parseExpr sourceName "*") == Right (Atom "*")
    it "parses atoms" $ property $
      \(ALispValAtom s) -> (parse parseExpr sourceName s) == Right (Atom s)
    it "should not parse the -79e symbol" $
      (parse parseExpr sourceName "-e79") == Right (List [Atom "-", Atom "e79"])
    it "parses positive integers" $ property $
      \k -> (k >= 0) ==> (parse parseExpr sourceName (show k)) == (Right (Number k))
    it "parses negative integers" $ property $
      \k -> (k < 0) ==> (parse parseExpr sourceName (show k)) == (Right (List [Atom "-", Number (-k)]))
    it "parses the empty string" $ do
      (parse parseExpr sourceName "\"\"" == (Right (String "")))
    it "parses spaces" $ do
      (parse parseExpr sourceName "\"  \"" == (Right (String "  ")))
    it "parses arbitrary strings" $ property $
      \(ALispValString s) -> (parse parseExpr sourceName (show s)) == (Right (String s))
    describe "Booleans" $ do
      it "parses the #t symbol" $ do
        parse parseExpr sourceName "#t" `shouldBe` (Right (Bool True))
      it "parses the #f symbol" $ do
        parse parseExpr sourceName "#f" `shouldBe` (Right (Bool False))
    describe "Lists" $ do
      it "parses the empty list" $ do
        parse parseExpr sourceName "()" `shouldBe` (Right (List []))
      it "parses the one element list" $ do
        parse parseExpr sourceName "(0)" `shouldBe` (Right (List [Number 0]))
      it "parses the one element list" $ do
        parse parseExpr sourceName "(1)" `shouldBe` (Right (List [Number 1]))
      it "parses the one element nested list" $ do
        parse parseExpr sourceName "((1))" `shouldBe` (Right (List [(List [Number 1])]))
    describe "Nil" $ do
      it "parses '()" $ do
        parse parseExpr sourceName "'()" `shouldBe` (Right nil)

    describe "Lisp Expr" $ do
      it "parses any lisp expr" $ property $
        \(ALispVal lispVal) ->
          lispValWithPositiveIntegers lispVal ==>
            (parse parseExpr sourceName (show lispVal)) == (Right lispVal)
      it "parses nested lists" $ do
        parse parseExpr sourceName "((1 2))" `shouldBe` (Right (List [List [Number 1, Number 2]]))
      it "parses nested dotted lists" $ do
        parse parseExpr sourceName "((1 . 2) . 0)" `shouldBe` (Right (DottedList [DottedList [Number 1] (Number 2)] (Number 0)))

    describe "Dotted Lists" $ do
      it "parses 2 elements dotted lists" $
        parse parseExpr sourceName "(1 . 2)" `shouldBe` (Right (DottedList [Number 1] (Number 2)))

    describe "Lisp quoted values" $ do
      it "parses quoted empty list" $ do
        parse parseExpr sourceName "'()" `shouldBe` (Right (List []))
      it "parses quoted booleans" $ property $
        \b -> (parse parseExpr sourceName ("'" ++ (show (Bool b)))) == (Right (List [Atom "quote", (Bool b)]))
      it "parses quoted positive numbers" $ property $
        \k -> (k >= 0) ==> (parse parseExpr sourceName ("'" ++ (show (Number k)))) == (Right (List [Atom "quote", (Number k)]))
      it "parses quoted negative numbers" $ property $
        \k -> (k < 0) ==> (parse parseExpr sourceName ("'" ++ (show (Number k)))) == (Right (List [Atom "quote", (List [Atom "-", Number (-k)])]))
      it "parses quoted exprs" $ property $
        \(ALispVal lispVal) ->
          lispValWithPositiveIntegers lispVal
            ==> (parse parseExpr sourceName ("'" ++ (show lispVal))) == (Right (List [Atom "quote", lispVal]))

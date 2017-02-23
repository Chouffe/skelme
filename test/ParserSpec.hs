module ParserSpec where

import           Test.Hspec
import           Test.QuickCheck               (elements, forAll, property,
                                                (==>))
import           Text.ParserCombinators.Parsec (parse)

import           Data
import           Parser                        (parseDottedList, parseExpr,
                                                parseList, parseNil, parseNumber,
                                                parseQuoted, symbol)
import           SpecUtils

sourceName :: String
sourceName = "ParserSpec"

specs :: Spec
specs =
  describe "Parser" $ do

    describe "Numbers" $ do
      it "parses integers" $ property $
        \k -> (parse parseNumber sourceName (show k)) == (Right (Number k))

    describe "Lisp Symbols" $ do
      it "parses symbols" $ property $
        forAll (elements "!#$%&|*+-/:<=>?@^_~") $
          \s -> (parse symbol sourceName [s]) == (Right s)

    describe "Lisp Booleans" $ do
      it "parses the #t symbol" $ do
        parse parseExpr sourceName "#t" `shouldBe` (Right (Bool True))
      it "parses the #f symbol" $ do
        parse parseExpr sourceName "#f" `shouldBe` (Right (Bool False))

    describe "Lisp Lists" $ do
      it "parses the empty list" $ do
        parse parseList sourceName "()" `shouldBe` (Right (List []))
      it "parses the one element list" $ do
        parse parseList sourceName "(0)" `shouldBe` (Right (List [Number 0]))

    describe "Lisp Dotted Lists" $ do
      it "parses the 2 elements dotted list" $ do
        parse parseDottedList sourceName "(1 . 2)" `shouldBe` (Right (DottedList [Number 1] (Number 2)))

    describe "Lisp expr" $ do
      -- FIXME: run test suite and work on the failed case
      it "parses any lisp expr" $ property $
        \(ALispVal lispVal) -> (parse parseExpr sourceName (show lispVal)) == (Right lispVal)
      it "parses nested lists" $ do
        parse parseExpr sourceName "((1 2))" `shouldBe` (Right (List [List [Number 1, Number 2]]))
      it "parses nested dotted lists" $ do
        parse parseExpr sourceName "((1 . 2) . 0)" `shouldBe` (Right (DottedList [DottedList [Number 1] (Number 2)] (Number 0)))

    describe "Lisp nil values" $ do
      it "parses '() as the empty list" $ do
        parse parseNil sourceName "'()" `shouldBe` (Right (List []))
      it "parses nil as the empty list" $ do
        parse parseNil sourceName "nil" `shouldBe` (Right (List []))

    describe "Lisp quoted values" $ do
      it "parses quoted empty list" $ do
        parse parseExpr sourceName "'()" `shouldBe` (Right (List []))
      it "parses quoted booleans" $ property $
        \b -> (parse parseQuoted sourceName ("'" ++ (show (Bool b)))) == (Right (List [Atom "quote", (Bool b)]))
      it "parses quoted positive numbers" $ property $
        \k -> (k >= 0) ==> (parse parseQuoted sourceName ("'" ++ (show (Number k)))) == (Right (List [Atom "quote", (Number k)]))
      it "parses quoted negative numbers" $ property $
        \k -> (k <= 0) ==> (parse parseQuoted sourceName ("'" ++ (show (Number k)))) == (Right (List [Atom "quote", (Number k)]))
      -- FIXME: run test suite and work on the failed case
      it "parses quoted exprs" $ property $
        \(ALispVal lispVal) -> (parse parseQuoted sourceName ("'" ++ (show lispVal))) == (Right (List [Atom "quote", lispVal]))

-- FIXME:
-- Failed parsing lib expr: (6 #f "lcvfyvd" -6ba$/+by:b (:e!#15*0 "" ez !4/&h: 9 "daalivyq" . -6))

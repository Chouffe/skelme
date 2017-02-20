module ParserSpec where

import Test.QuickCheck (property, forAll, elements)
import Test.Hspec
import Text.ParserCombinators.Parsec  (parse)

import Data
import Parser
  ( parseNumber
  , symbol
  , parseList
  , parseDottedList
  , parseExpr
  , parseQuoted
  )

-- TODO: add arbitrary instance on LispVal + show instance

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

    describe "Lisp Lists" $ do
      it "parses the empty list" $ do
        parse parseList sourceName "()" `shouldBe` (Right (List []))
      it "parses the one element list" $ do
        parse parseList sourceName "(0)" `shouldBe` (Right (List [Number 0]))

    describe "Lisp Dotted Lists" $ do
      it "parses the 2 elements dotted list" $ do
        parse parseDottedList sourceName "(1 . 2)" `shouldBe` (Right (DottedList [Number 1] (Number 2)))

    describe "Lisp quoted values" $ do
      it "parses quoted numbers" $ property $
        \k -> (parse parseQuoted sourceName ("'" ++ (show k))) == (Right (List [Atom "quote", (Number k)]))
      it "parses quoted exprs" $
        pendingWith "need to add arbitrary instance for LispVal"

    describe "Lisp Booleans" $ do
      it "parses the #t symbol" $ do
        parse parseExpr sourceName "#t" `shouldBe` (Right (Bool True))
      it "parses the #f symbol" $ do
        parse parseExpr sourceName "#f" `shouldBe` (Right (Bool False))

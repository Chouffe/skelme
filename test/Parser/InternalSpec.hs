module Parser.InternalSpec where


import           Test.Hspec
import           Test.QuickCheck               (property, (==>))
import           Text.ParserCombinators.Parsec (parse)

import           Data
import           Parser.Internal               (parseAtom, parseDottedList,
                                                parseList, parseNumber,
                                                parseQuoted, parseString)
import           SpecUtils

sourceName :: String
sourceName = "Parser.Internal.Spec"

specs :: Spec
specs =
  describe "Parser.Internal" $ do

    describe "Strings" $ do
      it "parses the empty string" $ do
        (parse parseString sourceName "\"\"" == (Right (String "")))
      it "parses spaces" $ do
        (parse parseString sourceName "\"  \"" == (Right (String "  ")))
      it "parses arbitrary expr strings" $ property $
        \(ALispValString s) -> (parse parseString sourceName (show s)) == (Right (String s))

    describe "Numbers" $ do
      it "parses positive numbers" $ property $
        \k -> (k >= 0) ==> (parse parseNumber sourceName (show k)) == (Right (Number k))
      it "parses negative numbers" $ property $
        \k -> (k < 0) ==> (parse parseNumber sourceName (show k)) == (Right (List [Atom "-", (Number (-k))]))

    describe "Atoms" $ do
      it "parses arbitrary atoms" $
        property $ \(ALispValAtom s) -> (parse parseAtom sourceName s) == Right (Atom s)

    describe "Booleans" $ do
      it "parses the #t symbol" $ do
        parse parseAtom sourceName "#t" `shouldBe` (Right (Bool True))
      it "parses the #f symbol" $ do
        parse parseAtom sourceName "#f" `shouldBe` (Right (Bool False))

    describe "Quoted Forms" $ do
      it "parses quoted booleans" $ property $
        \b -> (parse parseQuoted sourceName ("'" ++ (show (Bool b)))) == (Right (List [Atom "quote", (Bool b)]))
      it "parses quoted positive numbers" $ property $
        \k -> (k >= 0) ==> (parse parseQuoted sourceName ("'" ++ (show (Number k)))) == (Right (List [Atom "quote", (Number k)]))
      it "parses quoted negative numbers" $ property $
        \k -> (k < 0) ==> (parse parseQuoted sourceName ("'" ++ (show (Number k)))) == (Right (List [Atom "quote", (List [Atom "-", Number (-k)])]))
      it "parses quoted exprs" $ property $
        \(ALispVal lispVal) ->
          lispValWithPositiveIntegers lispVal ==>
            (parse parseQuoted sourceName ("'" ++ (show lispVal))) == (Right (List [Atom "quote", lispVal]))

    describe "Lists" $ do
      it "parses the empty list" $ do
        parse parseList sourceName "()" `shouldBe` (Right (List []))
      it "parses the one element list" $ do
        parse parseList sourceName "(0)" `shouldBe` (Right (List [Number 0]))
      it "parses the one element list" $ do
        parse parseList sourceName "(1)" `shouldBe` (Right (List [Number 1]))
      it "parses the one element nested list" $ do
        parse parseList sourceName "((1))" `shouldBe` (Right (List [(List [Number 1])]))
      -- FIXME: too hard to generate elements without negative numbers
      it "parses lists" $ property $
        \(ALispValList lispVals) ->
            all lispValWithPositiveIntegers lispVals ==>
              parse parseList sourceName (show (List lispVals)) == Right (List lispVals)

    describe "Dotted Lists" $ do
      it "parses the 2 elements dotted list" $ do
        parse parseDottedList sourceName "(1 . 2)" `shouldBe` (Right (DottedList [Number 1] (Number 2)))
      -- FIXME: too hard to generate elements without negative numbers
      it "parses dotted lists" $ property $
        \(ALispValList lispVals) (ALispVal lispVal) ->
          length lispVals > 0 && all lispValWithPositiveIntegers (lispVal:lispVals) ==>
            parse parseDottedList sourceName (show (DottedList lispVals lispVal)) == Right (DottedList lispVals lispVal)

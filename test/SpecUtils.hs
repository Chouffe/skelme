module SpecUtils where

import           Control.Applicative (liftA2)
import           Data
import           Test.QuickCheck

newtype ALispVal = ALispVal LispVal deriving (Eq, Show)
newtype ALispValList = ALispValList [LispVal] deriving (Eq, Show)
newtype ALispValString = ALispValString String deriving (Eq, Show)
newtype ALispValAtom = ALispValAtom String deriving (Eq, Show)
newtype ALispValPrimitive = ALispValPrimitive LispVal deriving (Eq, Show)

symbolGen :: Gen Char
symbolGen = elements "!#$%&|*+-/:<=>?@^_~"

symbolFirstLetterGen :: Gen Char
symbolFirstLetterGen = elements "!#$%&|*+/:<=>?@^_~"

letterGen :: Gen Char
letterGen = elements "abcdefghijklmnopqrstuvwxyz"

stringGen :: Gen String
stringGen = listOf letterGen

positiveDigitGen :: Gen Integer
positiveDigitGen = elements [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

digitToChar :: Integer -> Char
digitToChar 0 = '0'
digitToChar 1 = '1'
digitToChar 2 = '2'
digitToChar 3 = '3'
digitToChar 4 = '4'
digitToChar 5 = '5'
digitToChar 6 = '6'
digitToChar 7 = '7'
digitToChar 8 = '8'
digitToChar 9 = '9'
digitToChar _ = '0'

lispAtomGen :: Gen String
lispAtomGen = do
  firstChar <- oneof [symbolFirstLetterGen, letterGen]
  restString <- listOf $ oneof [symbolGen, letterGen, digitToChar <$> positiveDigitGen]
  return $ firstChar : restString

sizedLispValGen :: Int -> Gen LispVal
sizedLispValGen m = frequency $
  [ (1, (Bool <$> arbitrary))
  , (1, (Number <$> arbitrary))
  , (1, (Atom <$> lispAtomGen))
  , (1, (String <$> stringGen))
  , (k, (List <$> listOf (sizedLispValGen m')))
  , (k, (liftA2 DottedList (listOf1 (sizedLispValGen m')) (sizedLispValGen m')))
  ]
  where m' = m `div` 10
        k = min m 1

lispValGen :: Gen LispVal
lispValGen = sized sizedLispValGen

primitiveGen :: Gen LispVal
primitiveGen = frequency $
  [ (1, (Bool <$> arbitrary))
  , (1, (Number <$> arbitrary))
  , (1, (String <$> stringGen))
  ]

shrinkLispVal :: LispVal -> [LispVal]
shrinkLispVal (Bool b) = map Bool $ shrink b
shrinkLispVal (Number n) = map Number $ shrink n
shrinkLispVal (String s) = map String $ shrink s
shrinkLispVal (Atom (h:t)) = map (Atom . (h:)) $ shrink t
shrinkLispVal (List xs) = [List [shrinked] | x <- xs, shrinked <- shrinkLispVal x]
shrinkLispVal (DottedList as a) = [DottedList [shrinked] y | x <- as, shrinked <- shrinkLispVal x, y <- shrinkLispVal a]
shrinkLispVal _ = []

instance Arbitrary ALispVal where
  arbitrary = ALispVal <$> lispValGen
  shrink (ALispVal lispVal) = map ALispVal $ shrinkLispVal lispVal

instance Arbitrary ALispValList where
  arbitrary = ALispValList <$> listOf lispValGen
  shrink (ALispValList lispVals) = map (ALispValList . shrinkLispVal) lispVals

-- TODO: add shrinking
instance Arbitrary ALispValString where
  arbitrary = ALispValString <$> stringGen

-- TODO: add shrinking
instance Arbitrary ALispValAtom where
  arbitrary = ALispValAtom <$> lispAtomGen

-- TODO: add shrinking
instance Arbitrary ALispValPrimitive where
  arbitrary = ALispValPrimitive <$> primitiveGen

lispValWithPositiveIntegers :: LispVal -> Bool
lispValWithPositiveIntegers (Number k) = k >= 0
lispValWithPositiveIntegers (Atom "-") = False
lispValWithPositiveIntegers (List xs) = all lispValWithPositiveIntegers xs
lispValWithPositiveIntegers (DottedList xs x) =
  lispValWithPositiveIntegers x && all lispValWithPositiveIntegers xs
lispValWithPositiveIntegers _ = True


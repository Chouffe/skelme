module Data where

import           Data.List (intercalate)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
  deriving (Eq)

instance Show LispVal where
  show (Atom name) = name
  show (List xs) = "(" ++ (intercalate " " (map show xs)) ++ ")"
  show (DottedList hs t) = "(" ++ (intercalate " " (map show hs)) ++ " . " ++ show t ++ ")"
  show (Number k) = show k
  show (String s) = "\"" ++ s ++ "\""
  show (Bool b) = case b of
                    True  -> "#t"
                    False -> "#f"

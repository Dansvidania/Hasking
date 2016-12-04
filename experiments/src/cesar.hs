module Cesar where

import Test.QuickCheck
import Data.Bool (bool)
import Data.Char

cesar :: Int -> String -> String
cesar _ "" = ""
cesar i xs = map (shiftLetter i) xs

letters :: [Char]
letters = ['a'..'z']++['A'..'Z']

shiftLetter :: Int -> Char -> Char
shiftLetter i x
    | x `elem` letters = (chr . (+ (base x)) . (flip mod 25) . (+ i) . (flip (-)(base x))) $ ord x
    | otherwise = x 
        where base y = bool 65 97 (y `elem` ['a'..'z'])




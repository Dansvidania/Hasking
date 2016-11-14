module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

dropLastDigit x = div x 10

digits :: Int -> [Int]
digits x = reverse $ split x
    where split x
            | x < 10 = [x]
            | otherwise = u : split xs
                where xs = fst $ divMod x 10
                      u = snd $ divMod x 10

wordNumber :: Int -> String
wordNumber x = concat $ intersperse "-" $ map digitToWord (digits x)

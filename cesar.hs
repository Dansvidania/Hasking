module Cesar where

import Data.Bool (bool)
import Data.Char

cesar :: Int -> String -> String
cesar _ "" = ""
cesar i xs = map (shiftLetter i) xs
    where letters = ['a'..'z']++['A'..'Z']
          shiftLetter i x
            | x `elem` letters = (chr . (+ (base x)) . (flip mod 25) . (+ i) . (flip (-)(base x))) $ ord x
            | otherwise = x 
                where base x = bool 65 97 (x `elem` ['a'..'z'])

module DaPhone ( stringToKeyTaps
               , fingerTaps
               , mostPopularLetter
               , mostPopularWord
               , coolestLtr
               , coolestWord
               , daPhoneTaps
               , daPhoneTaps
               , testKeyBoard
               , testConvo) where

import Data.Char
import Data.List
import Data.Maybe

type KeyLabel = Char
type Digit = Char

data Key = Key KeyLabel [Digit] deriving (Eq, Show)
label (Key l _) = l
digits (Key _ ds) = ds

isDigitOf :: Digit -> Key -> Bool
c `isDigitOf` key = c `elem` (digits key)

type KeyboardLayout = [Key]

getKeyFor :: KeyboardLayout -> Digit -> Key
getKeyFor kmap c = fromJust $ find (c `isDigitOf`) kmap


type Taps = Int
type KeyTaps = [(KeyLabel, Taps)]

stringToKeyTaps :: KeyboardLayout -> String -> KeyTaps
stringToKeyTaps _ [] = []
stringToKeyTaps kmap (c:cs)i
    | c `elem` ['A'..'Z'] =
    charToKeyTap kmap '^' : stringToKeyTaps kmap ((toLower c):cs)
    | c `elem` charSet = charToKeyTap kmap c : stringToKeyTaps kmap cs
    | otherwise = stringToKeyTaps kmap cs
    where charSet = foldr ((++) . (digits)) [] kmap

charToKeyTap kmap c =
        (label $ theKey, (fromJust $ elemIndex c $ digits $ theKey) + 1)
                        where theKey = getKeyFor kmap c

fingerTaps :: KeyTaps -> Taps
fingerTaps = foldr ((+) . snd) 0

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopularElement

mostPopularWord :: String -> String
mostPopularWord = mostPopularElement . words

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . filter (/=' ') . concat

coolestWord :: [String] -> String
coolestWord = mostPopularWord . intercalate " "

mostPopularElement :: (Ord a, Eq a) => [a] -> a
mostPopularElement = head . head .
        sortBy (\x y -> (length y) `compare` (length x)) . group . sort




-- provided variables
testKeyBoard :: KeyboardLayout
testKeyBoard = [ Key '1' "1"
               , Key '2' "abc2"
               , Key '3' "def3"
               , Key '4' "ghi4"
               , Key '5' "jkl5"
               , Key '6' "mno6"
               , Key '7' "pqrs7"
               , Key '8' "tuv8"
               , Key '9' "wxyz9"
               , Key '0' " 0"
               , Key '*' "^"
               , Key '#' ".,"
               ]

testConvo :: [String]
testConvo = [ "Wanna play 20 questions"
            , "Ya"
            , "U 1st haha"
            , "Lol ok. Have u ever tasted alcohol lol"
            , "Lol ya"
            , "Lol ur cool haha. Ur turn"
            ]

--partially applied converter for provided exercise
daPhoneTaps = stringToKeyTaps testKeyBoard

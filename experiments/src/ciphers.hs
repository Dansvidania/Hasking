module Ciphers (cesar, vignereEnc, vignereDec) where

import Test.QuickCheck
import Test.Hspec
import Data.Bool (bool)
import Data.Char

cesar :: Int -> String -> String
cesar _ "" = ""
cesar i xs = map (shiftLetter i) xs

letters :: [Char]
letters = ['a'..'z']++['A'..'Z']

shiftLetter :: Int -> Char -> Char
shiftLetter i x
    | x `elem` letters = 
        (chr . (+ (base x)) . (flip mod 26) . (+ i) . 
        (flip (-)(base x))) $ ord x
    | otherwise = x 

base :: Char -> Int
base y = bool 65 97 (y `elem` ['a'..'z'])

type Key = String
type Message = String

vignereEnc :: Key -> Message -> Message
vignereEnc "" m = m
vignereEnc k m = vig '+' (cycle k) m

vignereDec :: Key -> Message -> Message
vignereDec "" m = m
vignereDec k m = vig '-' (cycle k) m

vig :: Char -> Key -> Message -> Message
vig _ _ [] = []
vig w (k:ks) (c:cs) = shiftLetter sh c : vig w (keys) (cs)
    where kVal = ord k - base k
          sh = bool (-kVal) (kVal) (w == '+')
          keys = bool (k:ks) (ks) (c `elem` letters)


-- tests
vignereIdentityProperty :: String -> String -> Bool
vignereIdentityProperty k m = m == (vignereDec k . vignereEnc k) m

cesarIdentityProperty :: Int -> String -> Bool
cesarIdentityProperty i m = m == (cesar (-i) . cesar i) m


runTests :: IO ()
runTests = hspec $ do
        describe "vigenere cipher" $ do
        it "string stay same after encode and decode with vignere" $ do
            property $ vignereIdentityProperty
        it "string stay same after encode and decode with cesar" $ do
            property $ cesarIdentityProperty

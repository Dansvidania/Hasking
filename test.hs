{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Char
import Data.Bool

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    TisAn x == TisAn y = x == y

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    Two x y == Two w z = x == w && y == z

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
    TisAnInt x == TisAnInt y = x == y
    TisAString x == TisAString y = x == y
    _ == _ = False

data Pair a = Pair a a
instance Eq a => Eq ( Pair a ) where
    Pair x y == Pair w z = x == w && y == z
myZip :: [a] -> [b] -> [(a,b)]
myZip = myZipWith (,) 

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys   

takeUpper :: String -> String
takeUpper = filter (isUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

capsLock :: String -> String
capsLock [] = []
capsLock (x:xs) = toUpper x : capsLock xs

capsFirst :: String -> Char
capsFirst = head . capitalize

myOr :: [Bool] -> Bool
myOr [] = False
myOr (b:bs)
    | b == True = True
    | otherwise = myOr bs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny p (x:xs)
    | p x = True
    | otherwise = myAny p xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
    | e == x = True
    | otherwise = myElem e xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squish' :: [[a]] -> [a]
squish' = squishMap id



cesar :: Int -> String -> String
cesar _ "" = ""
cesar i xs = map (shiftLetter i) xs
    where shiftLetter i x
            | code `elem` [97..122] = shiftInRange 122 i code
            | code `elem` [65..90] = shiftInRange 90 i code
            | otherwise = x
                where code = ord x
                      shiftInRange top i = chr . (flip mod (top+1)) . (+(top+i)) 


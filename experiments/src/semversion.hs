{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Semver where

import Data.ByteString (ByteString)
import Control.Applicative ((<|>), liftA3)
import Text.Trifecta
import Data.String (IsString, fromString)
import Data.Map (Map)
import qualified Data.Map as M
import Text.RawString.QQ
import Data.Word
import Text.Printf

ver :: ByteString
ver = "12.12.1987-alpha"

type Major = Integer

type Minor = Integer

type Patch = Integer

newtype SemString =
  SemString String
  deriving (Eq)

instance IsString SemString where
  fromString s = SemString s

instance Show SemString where
  show (SemString x) = x

instance Ord SemString where
  compare "" _ = GT
  compare _ "" = LT
  compare (SemString x) (SemString y) = compare x y

data SemVersion =
  Version Major
          Minor
          Patch
          SemString
  deriving (Eq, Ord)

instance Show SemVersion where
  show (Version a b c "") = show a ++ "." ++ show b ++ "." ++ show c
  show (Version a b c d) = show a ++ "." ++ show b ++ "." ++ show c ++ "-" ++ show d

parseWith
  :: Show a
  => Parser a -> String -> IO ()
parseWith p s = print $ parseString p mempty s

parseVersion :: Parser SemVersion
parseVersion = do
  m <- parseVerInt
  n <- parseVerInt
  p <- parseVerInt
  s <- parseVerStr
  return (Version m n p (SemString s))

parseVerInt :: Parser Integer
parseVerInt = do
  int <- integer
  skipMany (char '.')
  return int

parseVerStr :: Parser String
parseVerStr = try (char '-' >> (some letter)) <|> return ""

testList :: [SemVersion]
testList =
  [ Version 12 12 1987 ""
  , Version 12 12 1988 ""
  , Version 12 12 1990 ""
  , Version 12 12 1987 "alpha"
  , Version 12 12 1987 "alpha.1"
  ]

parseElemOf :: [Char] -> Parser Char
parseElemOf (x:xs) = foldr (<|>) (char x) (map (char) xs)

strings :: [String] -> Parser String
strings (x:xs) = foldr (<|>) (string x) (map (string) xs)

parseDigit :: Parser Char
parseDigit =
  let lst = ['0' .. '9']
  in parseElemOf lst

base10Integer :: Parser Integer
base10Integer = do
  str <- some parseDigit
  return (read str)

base10Integer' :: Parser Integer
base10Integer' = do
  (char '-' >> base10Integer >>= (return . negate)) <|> base10Integer

type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange
              LineNumber
  deriving (Eq, Show)

rollTry
  :: Show a
  => Parser a -> Parser a
rollTry p = try p <|> (anyChar >> rollTry p)

getNDigits :: Int -> Parser Int
getNDigits n = do
  x <- rollTry $ count n parseDigit
  return (read x)

parsePhone :: Parser PhoneNumber
parsePhone = do
  a <- getNDigits 3
  b <- getNDigits 3
  c <- getNDigits 4
  return $ PhoneNumber a b c

phone :: Parser PhoneNumber
phone = liftA3 PhoneNumber (getNDigits 3) (getNDigits 3) (getNDigits 4)

eol :: Parser ()
eol = do
  _ <- oneOf "\n\r"
  return ()

type Date = String

type Hour = (Int, Int)

type Started = Hour

type Activity = (Hour, String)

data DayActivities =
  Day Date
      [Activity]
  deriving (Eq, Show)

activityParser :: Parser Activity
activityParser = (,) <$> getHour <*> (char ' ' >> manyTill anyChar (eol <|> eof))
  where
    getHour :: Parser Hour
    getHour = rollTry $ (,) <$> (getNDigits 2) <*> (char ':' >> getNDigits 2)

dateParser :: Parser Date
dateParser = (rollTry $ string "# ") >> manyTill anyChar (eol <|> eof)

dayActParser :: Parser DayActivities
dayActParser = Day <$> dateParser <*> (some activityParser)

testString :: String
testString =
  [r| -- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
|]

data IPAddress =
  IPAddress Word32
  deriving (Eq, Show, Ord)

bToW32 :: String -> Word32
bToW32 "" = 0
bToW32 (b:bs)
  | b == '1' =
    let x = length bs
    in (2 ^ x) + bToW32 bs
  | otherwise = bToW32 bs

zeroFill :: String -> String
zeroFill s
  | length s < 8 = zeroFill ('0' : s)
  | otherwise = s

intListToBitS :: [Int] -> String
intListToBitS = concat . (map (zeroFill . printf "%b"))

ipV4Parser :: Parser IPAddress
ipV4Parser = do
  ls <- sepBy ipValue (char '.')
  let lst :: [Int]
      lst = map read ls
  return $ IPAddress $ bToW32 $ intListToBitS lst
  where
    ipValue = strings (map show [255,254 .. 0])

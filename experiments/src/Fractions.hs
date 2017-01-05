{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
import Data.Attoparsec.Text (parseOnly)
import Data.String (IsString)

badFraction
  :: IsString s
  => s
badFraction = "1/0"

alsoBad
  :: IsString s
  => s
alsoBad = "10"

shouldWork
  :: IsString s
  => s
shouldWork = "1/2"

shouldAlsoWork
  :: IsString s
  => s
shouldAlsoWork = "2/1"

parseFraction
  :: (Monad m, TokenParsing m)
  => m Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "denominator cannot be 0"
    _ -> return (numerator % denominator)

testParseFraction = parseString parseFraction mempty

tpf = testParseFraction

tpDoF = parseString decimalOrFraction mempty

decimalOrFraction :: Parser (Either Integer Rational)
decimalOrFraction = (try $ Right <$> parseFraction) <|> (Left <$> integer)

attoParse = parseOnly parseFraction

printAttoParse = print . attoParse

main :: IO ()
main = do
  printAttoParse badFraction
  printAttoParse shouldWork
  printAttoParse shouldAlsoWork
  printAttoParse alsoBad
  print $ "now it's trifecta"
  print $ tpf shouldWork
  print $ tpf shouldAlsoWork
  print $ tpf alsoBad
  print $ tpf badFraction

module LearnParsers where

import Text.Trifecta
import Text.Trifecta.Parser
import Control.Applicative
import Control.Monad
import Data.Monoid ((<>))
import GHC.IO.Handle.FD

stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof

one' = one >> eof

oneTwo = char '1' >> char '2' >> eof

oneTwo' = oneTwo >> eof

oneTwoThree = char '1' >> char '2' >> char '3' >> eof

testParse :: Parser () -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

parseTXT :: Parser String -> String -> IO ()
parseTXT p txt = print $ parseString p mempty txt

parse123 :: Parser String
parse123 = do
  a <- string "1"
  b <- (liftM2 (<>) (string "2") (string "3" <|> return "")) <|> (return "")
  return (a <> b)

parse123' :: Parser String
parse123' = do
  one <- string "1"
  two <- ifOk one (string "2")
  thr <- ifOk two (string "3")
  return (one <> two <> thr)
  where
    empty = return ""
    ifOk x p =
      case x of
        _:_ -> p <|> empty
        _ -> empty

parse123'' :: Parser String
parse123'' =
  liftM2 (<>) (string "1") (liftM2 (<>) (string "2") (string "3" <|> return "") <|> return "")

test = parseTXT parse123'

string' :: String -> Parser String
string' = (sequenceA . map char)

one23eof :: Parser Integer
one23eof = do
  x <- integer
  eof
  return (x)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "..tree:"
  testParse oneTwoThree

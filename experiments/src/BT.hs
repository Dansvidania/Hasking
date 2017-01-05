{-# LANGUAGE OverloadedStrings #-}

module BT where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, parseTest)
import Data.String (IsString)

trifP
  :: Show a
  => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP
  :: Show a
  => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP
  :: Show a
  => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

nobackParse
  :: (Monad f, CharParsing f)
  => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse
  :: (Monad f, CharParsing f)
  => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

a
  :: IsString s
  => s
a = "13"

tryAnnot
  :: (Monad f, CharParsing f)
  => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")

main :: IO ()
main
-- trifecta
 = do
  trifP nobackParse a
  trifP tryParse a
  -- parsec
  parsecP nobackParse a
  parsecP tryParse a
  -- attoparsec 
  attoP nobackParse a
  attoP tryParse a

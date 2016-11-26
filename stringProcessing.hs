
import Data.Bool

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe =  unwords . rplcThe . words where
    rplcThe [] = []
    rplcThe (w:ws) = bool w "a" (w =="the") : rplcThe ws


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countTheBfrVwl . words

countTheBfrVwl :: [String] -> Integer
countTheBfrVwl [] = 0
countTheBfrVwl (x:[]) = 0
countTheBfrVwl (w:y:ws)
    | w == "the" && (head y) `elem` "aeiou" = 1 + countTheBfrVwl ws
    | otherwise = countTheBfrVwl (y:ws)


countVowels :: String -> Integer
countVowels = fromIntegral . length . filter (`elem` "AEIOUaeiou")

countCons :: String -> Integer
countCons = fromIntegral . length . filter (not . (`elem` "AEIOUaeiou ")) 

newtype Word' =
        Word' String 
        deriving (Show, Eq, Ord)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str
    | vowels > consonants = Nothing
    | otherwise = Just (Word' str)
    where vowels = countVowels str
          consonants = countCons str

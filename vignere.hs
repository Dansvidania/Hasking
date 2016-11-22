
import Data.Bool
import Data.Char


-- FIRST ATTEMPT OF IMPLEMENTATION KEEPING CHAR CASE THE SAME

shiftVLetter :: Char -> Char -> Char
shiftVLetter k l
            | l `elem` letters = ( chr . (+ (base l)) . (flip mod 26) . (+ shift k) . (flip (-)(base l))) $ ord l
            | otherwise = l
                where base l = bool 65 97 (l `elem` ['a'..'z'])
                      shift k = (ord k) - (base k)
                      letters = ['a'..'z'] ++ ['A'..'Z']


vigLst :: String -> String -> [String]
vigLst _ [] = []
vigLst key msg =
        let fstWord = takeWhile (/= ' ')
            firstWord = fstWord msg
            len = length firstWord
            cycledKey = cycle key in
            zipWith (shiftVLetter) (cycledKey) (firstWord) : (vigLst (drop len cycledKey) (drop (len+1) msg))
            

vignere' :: String -> String -> String
vignere' key msg = unwords $ vigLst key msg



-- SECOND ATTEMPT OF IMPLEMENTATION (No spaces in encoded strings)

vigShift :: (Int -> Int -> Int) -> Char -> Char -> Char
vigShift f k l
    | isLetter l = chr . (+(base l)) $ ((ord l) `f` (ord k)) `mod` 26
    | otherwise = l
        where base l = bool 97 65 (l `elem` ['A'..'Z'])
              isLetter l = l `elem` (['A'..'Z']++['a'..'z'])

encrypt = vigShift (+)
decrypt = vigShift (-)

type Key = String
type Message = String
type EncodedMessage = String

vignere :: (Char -> Char -> Char) -> Key -> Message -> EncodedMessage
vignere f key msg = zipWith f (cycle key) filteredInput
        where filteredInput = filter (/=' ') msg


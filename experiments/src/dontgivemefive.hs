import Data.Bool

dontGiveMeFive :: Int -> Int -> Int
dontGiveMeFive start end = length $ filter (not . hasFive) [start .. end]

dontGiveMeFive' :: Int -> Int -> Int
dontGiveMeFive' s e = length $ filter (not . hasFive) [s .. e]

hasFive :: Integral a => a -> Bool
hasFive x
  | x < 10 = x == 5
  | otherwise = let (next, lst) = divMod x 10
                 in (lst == 5) || hasFive next

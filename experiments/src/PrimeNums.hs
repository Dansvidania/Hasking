primesUpToN :: Integer -> [Integer]
primesUpToN n = primesIn [3,2] [4..n]

primesIn ps ns = case (filter (primeFltr ps) ns) of (x:xs) -> primesIn (x:ps) xs
                                                    ([]) -> ps  
                    
divisible = (\x y -> y `mod` x == 0)
notDivisibleBy = (\x y -> x `mod` y /= 0)

(!%) = notDivisibleBy

primeFltr :: [Integer] -> Integer -> Bool
primeFltr ls x = runAnd $ foldMap And $ liftA2 (!%) (pure x) ls

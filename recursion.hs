sumOneTo :: (Eq a, Num a) => a -> a
sumOneTo 0 = 0
sumOneTo n = n + sumOneTo (n - 1)

times :: (Eq a, Num a) => a -> a -> a
x `times` 0 = 0
x `times` y = x + (times x (y-1))

mc91 :: (Integral a) => a -> a
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 . mc91 $ x + 11

{-# LANGUAGE NoMonomorphismRestriction #-}

f :: Float
f = 1.0

g :: Fractional a => a
g = 1.0

--h :: Float
h :: RealFrac a => a
h = 1

--freud :: a -> a
freud :: Ord a => a -> a
freud x = x

freud' :: a -> a
-- freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

--sigmund :: Int -> Int
sigmund :: a -> Int
sigmund x = myX

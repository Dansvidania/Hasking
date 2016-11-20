{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
        tooMany :: a -> Bool

instance TooMany Int where
        tooMany x = x > 42

newtype Tm a = Tm a

instance (Ord a, Num a) => TooMany (Tm a) where
        tooMany n = n > 42

newtype Goats = 
    Goats Int deriving (Eq, Show, TooMany, Num, Ord)

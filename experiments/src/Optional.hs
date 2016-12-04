module Optional where

import Test.QuickCheck

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
        mempty = Nada
        Nada `mappend` x = x
        x `mappend` Nada = x
        (Only x) `mappend` (Only y) = Only (x `mappend` y)

instance Arbitrary a => Arbitrary (Optional a) where
        arbitrary = do
            a <- arbitrary
            frequency $ [(1, return Nada), (3, return(Only a))]

module Optional where

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
        mempty = Nada
        Nada `mappend` x = x
        x `mappend` Nada = x
        (Only x) `mappend` (Only y) = Only (x `mappend` y)

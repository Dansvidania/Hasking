import Criterion.Main

newtype DList a = DL
  { unDL :: [a] -> [a]
  }

empty :: DList a
empty = DL $ (id)

{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DL $ (x :)

{-# INLINE singleton #-}

toList :: DList a -> [a]
toList dl = unDL dl []

{-# INLINE toList #-}

infixr `cons`

cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)

{-# INLINE cons #-}

infixl `snoc`

snoc :: DList a -> a -> DList a
snoc dl x = DL $ \n -> unDL dl [x] ++ n

{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL $ unDL xs . unDL ys

{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) ([n] ++ xs)

constructDList :: Int -> [Int]
constructDList i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (singleton n `append` xs)

main :: IO ()
main =
  defaultMain
    [bench "concat list" $ whnf schlemiel 123456, bench "concat dlist" $ whnf constructDList 123456]

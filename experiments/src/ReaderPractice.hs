module ReaderPractice where

import Control.Applicative
import Control.Monad
import Data.Monoid

newtype And = And { runAnd :: Bool } deriving (Show, Eq, Ord)

instance Monoid And where
    mempty = And True
    mappend (And True) (And True) = And True
    mappend _ _ = And False

x = [1, 2, 3]
y = [5, 6, 7]
z = [7, 8, 9]

lookups :: Eq a => a -> [(a, b)] -> Maybe b
lookups _ [] = Nothing
lookups k ((a, b):ms)
  | k == a = Just b
  | otherwise = lookup k ms

-- should return Just 1
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- should return Just 8
ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' k = lookup k $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Integral a => (a, a) -> a
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

seqA :: Integral a => a -> [Bool]
seqA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ( (,) <$> xs <*> ys )

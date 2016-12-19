--chapter 20 library functions

import           Control.Applicative (liftA2)
import           Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr ((||) . (== x)) False

newtype Min a = Min { getMin :: Maybe a } deriving (Eq, Show, Ord)

mkMin :: a -> Min a
mkMin x = Min { getMin = Just x }

instance Ord a => Monoid (Min a) where
        mempty = Min Nothing
        mappend (Min Nothing) m               = m
        mappend m (Min Nothing)               = m
        mappend (Min a) (Min b) = Min (liftA2 min a b)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = getMin . foldMap mkMin

newtype Max a = Max { getMax :: Maybe a } deriving (Show, Eq, Ord)

mkMax :: a -> Max a
mkMax x = Max { getMax = Just x }

instance Ord a => Monoid (Max a) where
        mempty = Max Nothing
        mappend (Max Nothing) m = m
        mappend m (Max Nothing) = m
        (Max a) `mappend` (Max b) = Max (liftA2 max a b)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMax . foldMap mkMax

null' :: Foldable t => t a -> Bool
null' = foldr ((&&) . const False) True

length' :: Foldable t => t a -> Int
length' = foldr ((+) . const 1) 0

toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

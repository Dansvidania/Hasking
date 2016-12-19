module List where

import Data.Monoid (Monoid, (<>))
import Control.Applicative (liftA2)
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil xs = xs
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Arbitrary a => Arbitrary (List a) where
        arbitrary = Cons <$> arbitrary <*> pure Nil

instance Eq a => EqProp (List a) where
        (=-=) = eq

instance Monoid (List a) where
        mempty = Nil
        mappend = append 

instance Functor List where
        fmap _ Nil = Nil
        fmap f (Cons x (xs)) = Cons (f x) (fmap f xs)

instance Applicative List where
        pure x = Cons x (Nil)
        Nil <*> _ = Nil
        (Cons f (fs)) <*> (xs) = fmap f xs `mappend` (fs <*> xs)

instance Foldable List where
        foldMap _ (Nil) = mempty
        foldMap f (Cons x xs) = f x <> (foldMap f xs)

instance Traversable List where
        traverse _ Nil = pure Nil
        traverse f (Cons x xs) = liftA2 Cons (f x) (traverse f xs)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' i (Cons x (xs))
    | i < 1 = Nil
    | otherwise = Cons x (take' (i-1) xs)

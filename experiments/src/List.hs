module List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import Control.Monad
import Data.Monoid

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Show)


instance Arbitrary a => Arbitrary (List a) where
        arbitrary = do
            a <- arbitrary
            return (Cons a Nil)
            
instance Eq a => EqProp (List a) where
        (=-=) = eq

append :: List a -> List a -> List a
append Nil xs = xs
append (Cons x xs) ys = Cons x $ xs `append` ys

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

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' i (Cons x (xs))
    | i < 1 = Nil
    | otherwise = Cons x (take' (i-1) xs)


instance Monad List where
        return = pure
        Nil >>= _ = Nil
        (Cons x xs) >>= f = f x `append` (xs >>= f)

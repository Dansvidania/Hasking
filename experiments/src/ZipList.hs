module ZipList where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import List

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
        arbitrary = do
            a <- arbitrary
            return (ZipList' a)

instance Eq a => EqProp (ZipList' a) where
        xs =-= ys = xs' `eq` ys'
            where xs' = let (ZipList' l) = xs
                            in take' 3000 l
                  ys' = let (ZipList' l) = ys
                            in take' 3000 l

instance Functor ZipList' where
        fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
        pure f = ZipList' (Cons f Nil)
        _ <*> ZipList' Nil = ZipList' Nil
        ZipList' Nil <*> _ = ZipList' Nil
        ZipList' (Cons f fs) <*> ZipList' (Cons x xs) = ZipList' (Cons (f x) (fs <*> xs))

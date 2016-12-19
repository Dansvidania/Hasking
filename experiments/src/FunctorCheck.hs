{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FunctorCheck where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import GHC.Generics

type IntToInt = Fun Int Int


newtype Identity a = Identity a deriving (Generic, Show, Eq)

instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = do
            a <- arbitrary
            return $ Identity a

instance Functor Identity where
        fmap g (Identity x) = Identity (g x)

instance Applicative Identity where
        pure = Identity
        Identity f <*> Identity x = Identity (f x)

instance Foldable Identity where
        foldMap f (Identity x) = f x

instance Traversable Identity where
        traverse f (Identity x) = Identity <$> f x

type IdentityIdentity = Identity Int -> Bool
type IdentityCompose = Identity Int -> IntToInt -> IntToInt -> Bool


data Two a= Two a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Two a) where
        arbitrary = do
            x <- arbitrary
            y <- arbitrary
            return (Two x y)

instance Functor (Two) where
        fmap f (Two x y) = Two (f x) (f y)

instance Applicative (Two) where
        pure x = Two x x
        (Two f g) <*> (Two x y) = Two (f x) (g y)

instance Eq a => EqProp (Two a) where
        Two x y =-= Two a b = property $ (x == a) && (y == b)


data Pair a b = Pair a b deriving (Generic, Eq, Show)

instance Functor (Pair a) where
        fmap f (Pair a b) = Pair a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return $ Pair a b

instance Monoid a => Applicative (Pair a) where
        pure = Pair mempty
        Pair x f <*> Pair y a = Pair (x <> y) (f a)

instance (Eq a, Eq b) => EqProp (Pair a b) where
        Pair x y =-= Pair a b = property $ x == a && y == b


data Four a b c d = Four a b c d deriving (Eq, Show, Generic)

instance Functor (Four a b c) where
        fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
        Arbitrary (Four a b c d) where
            arbitrary = do
                a <- arbitrary
                b <- arbitrary
                c <- arbitrary
                d <- arbitrary
                return $ Four a b c d


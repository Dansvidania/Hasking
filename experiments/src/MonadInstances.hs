module MonadInstances where

import           Control.Monad
import           Data.Monoid              (Monoid, (<>))
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes


data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
        fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
        pure = const NopeDotJpg
        NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
        return = pure
        NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
        arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
        (=-=) = eq


data PhbtEither a b = Lleft a
                    | Rright b
                    deriving (Eq, Show)

instance Functor (PhbtEither a) where
        fmap f (Rright x) = Rright (f x)
        fmap _ (Lleft x)  = Lleft x

instance Monoid a => Applicative (PhbtEither a) where
        pure = Rright
        (<*>) (Lleft a) (Lleft b) = Lleft (a <> b)
        (<*>) (Rright x) (Rright y) = Rright (x y)
        (<*>) (Lleft a) _ = Lleft a
        (<*>) _ (Lleft a) = Lleft a

instance Monoid a => Monad (PhbtEither a) where
        return = pure
        Lleft x >>= _ = Lleft x
        Rright x >>= f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither a b) where
        arbitrary = 
            oneof $ [ Lleft <$> arbitrary
                    , Rright <$> arbitrary ]

instance (Eq a, Eq b) => EqProp (PhbtEither a b) where
        (=-=) = eq


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
        fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
        pure = Identity
        Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
        return = pure
        Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
        (=-=) = eq

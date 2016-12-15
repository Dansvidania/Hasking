module Validation where
    
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a = Failure' e
                    | Success' a
                    deriving (Eq, Show)

instance Functor (Validation e) where
        fmap _ (Failure' e) = Failure' e
        fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
        pure = Success'
        Failure' e <*> Failure' g = Failure' (e `mappend` g)
        _ <*> Failure' e = Failure' e
        Failure' e <*> _ = Failure' e
        Success' a <*> Success' b = Success' (a b)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
        arbitrary = do
            a <- arbitrary
            e <- arbitrary
            frequency $ 
                [(1, return (Failure' e)), (3, return (Success' a))]

instance (Eq a, Eq e) => EqProp (Validation a e) where
    Success' x =-= Success' y = x `eq` y
    Failure' x =-= Failure' y = x `eq` y
    _ =-= _ = True `eq` False

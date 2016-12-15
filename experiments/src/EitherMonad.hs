import           Control.Applicative
import           Control.Monad
import           Data.Monoid              (Monoid, (<>))
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
        fmap _ (First x)  = First x
        fmap f (Second b) = Second (f b)

instance Monoid a => Applicative (Sum a) where
        pure = Second
        Second x <*> Second y = Second (x y)
        First x  <*> First y  = First (x <> y)
        First f  <*> _        = First f
        _        <*> First f  = First f

instance Monoid a => Monad (Sum a) where
        return                = pure
        First x >>= _         = First x
        Second x >>=  f = f x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
        arbitrary = do
            oneof $ [ arbitrary >>= (return . First)
                    , arbitrary >>= (return . Second)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
        (=-=) = eq

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FunctorCheck where


import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Hspec
import GHC.Generics

type TypeIdentity = Bool
type TypeComposition = Fun Int Int -> Fun Int Int -> Bool


checkFunctor :: forall f a. (Functor f) => String -> f a -> IO ()
checkFunctor description f = hspec $ do
    describe description $ do
        it "identity property" $ do
            property $ (functorIdentity :: f a -> TypeIdentity)
        it "composition property" $ do
            property $ (functorCompose' :: f a -> TypeComposition)


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (b -> c)
                                        -> (a -> b)
                                        -> f a
                                        -> Bool
functorCompose f g x = fmap (f . g) x == (fmap f . fmap g) x

functorCompose' :: (Eq (f c), Functor f) => f a
                                         -> Fun b c
                                         -> Fun a b
                                         -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = 
        (fmap (f . g) x) == (fmap f . fmap g $ x)

type IntToInt = Fun Int Int


newtype Identity a = Identity a deriving (Generic, Show, Eq)

instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = do
            a <- arbitrary
            return $ Identity a

instance Functor Identity where
        fmap g (Identity x) = Identity (g x)

type IdentityIdentity = Identity Int -> Bool
type IdentityCompose = Identity Int -> IntToInt -> IntToInt -> Bool

checkIdentity :: IO ()
checkIdentity = hspec $ do
        describe "Identity functor" $ do
            it "identity property" $ do
                property $ (functorIdentity :: IdentityIdentity)
            it "composition property" $ do
                property $ (functorCompose' :: IdentityCompose)



data Pair a b = Pair a b deriving (Generic, Eq, Show)

instance Functor (Pair a) where
        fmap f (Pair a b) = Pair a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return $ Pair a b

type PairIdentity = Pair Int Int -> Bool
type PairCompose = Pair Int Int -> IntToInt -> IntToInt -> Bool

checkPair :: IO ()
checkPair = hspec $ do
        describe "Pair functor" $ do
            it "identity property" $ do
                property $ (functorIdentity :: PairIdentity)
            it "composition property" $ do
                property $ (functorCompose' :: PairCompose)
                


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

type FourIdentity = Four Int Int Int Int -> Bool
type FourCompose = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool

checkFour :: IO ()
checkFour = hspec $ do
    describe "Four functor" $ do
        it "identity" $ do
            property $ (functorIdentity :: FourIdentity)
        it "composition" $ do
            property $ (functorCompose' :: FourCompose)








main :: IO ()
main = do
        checkIdentity
        checkPair
        checkFour




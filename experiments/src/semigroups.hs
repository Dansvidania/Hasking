-- Semigroup exercises Chapter 15

import Test.QuickCheck
import Data.Semigroup
import qualified Data.Monoid as M

associativity :: (Eq m, Semigroup m) => m -> m -> m -> Bool
associativity a b c = (a <> (b <> c)) == ((a <> b) <> c)



data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
        Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
        arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

checkTrivial :: IO ()
checkTrivial = quickCheck (associativity :: TrivialAssoc)



newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
        Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = do
            a <- arbitrary
            return (Identity a)

instance Arbitrary a => Arbitrary (Sum a) where
        arbitrary = do
            a <- arbitrary
            return (Sum a)

type IdentityAssoc = Identity (Sum Integer) -> Identity (Sum Integer)
                        -> Identity (Sum Integer) -> Bool

checkIdentity :: IO ()
checkIdentity = quickCheck (associativity :: IdentityAssoc)



data Two a b = Two a b deriving (Eq, Show)

instance (Num a, Num b) => Semigroup (Two a b) where
        (Two x y) <> (Two w z) = Two (x + w) (y + z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
        arbitrary = do
            a <- arbitrary
            b <- arbitrary
            return (Two a b)

type TwoAssoc = Two Int Int -> Two Int Int -> Two Int Int -> Bool

checkTwo :: IO ()
checkTwo = quickCheck (associativity :: TwoAssoc)


newtype Bconj = Bconj Bool deriving (Eq, Show)

instance Semigroup Bconj where
        (Bconj x) <> (Bconj y) = Bconj (x && y)

instance Arbitrary Bconj where
        arbitrary = do
            x <- arbitrary
            return (Bconj x)

type ConjAssoc = Bconj -> Bconj -> Bconj -> Bool

checkBconj :: IO ()
checkBconj = quickCheck (associativity :: ConjAssoc)


-- TODO
newtype Combine a b = Combine { unCombine :: (a -> b) }



newtype Mem s a = Mem {
                    runMem :: s -> (a,s)
                    }



instance M.Monoid a => M.Monoid (Mem s a) where
    mempty = Mem (\s -> (mempty, s))
    mappend (Mem f) (Mem g) = 
        Mem $ \x -> let gRes = g x
                        fRes = f $ snd gRes
                        in (M.mappend (fst fRes) (fst gRes), snd fRes)

f' = Mem $ \s -> ("hi", s + 1)





main :: IO ()
main = do
        putStr $ "Trivial:   "
        checkTrivial
        putStr $ "Identity:  "
        checkIdentity
        putStr $ "Two:       "
        checkTwo
        putStr $ "Bconj:     "
        checkBconj
        print $ runMem (f' `M.mappend` mempty) 0
        print $ runMem (mempty `M.mappend` f') 0
        print $ (runMem mempty 0 :: (String, Int))
        print $ runMem (f' `M.mappend` mempty) 0 == runMem f' 0
        print $ runMem (mempty `M.mappend` f') 0 == runMem f' 0

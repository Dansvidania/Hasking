{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import Data.Char

boop
  :: Num a
  => a -> a
boop = (* 2)

doop
  :: Num a
  => a -> a
doop = (+ 10)

bip
  :: Num a
  => a -> a
bip = boop . doop

bloop
  :: Num a
  => a -> a
bloop = fmap boop doop

bbop
  :: Num a
  => a -> a
bbop = (+) <$> boop <*> doop

-- \x -> (+) (boop x) :: a -> (a -> a)
duwop = liftA2 (+) boop doop

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledLambda :: [Char] -> ([Char], [Char])
tupledLambda = cap >>= (\f -> rev >>= (\s -> return (f, s)))

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
  f <- cap
  s <- rev
  return (f, s)

newtype Reader r a = Reader
  { runReader :: (r -> a)
  }

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure = Reader . const
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb $ ra r) $ r

ask :: Reader a a
ask = Reader id

newtype HumanName =
  HumanName String
  deriving (Eq, Show, Ord)

newtype DogName =
  DogName String
  deriving (Eq, Show, Ord)

newtype Address =
  Address String
  deriving (Eq, Show, Ord)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

getDog :: Person -> Dog
getDog = liftA2 Dog dogName address

myLiftA2
  :: Applicative f
  => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f

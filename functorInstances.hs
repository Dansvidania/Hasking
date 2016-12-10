{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- 1
data Quant a b = Finance
               | Desk a
               | Bloor b
               deriving (Eq, Show)
               
instance Functor (Quant a) where
        fmap f (Bloor b) = Bloor (f b)
        fmap _ Finance = Finance
        fmap _ (Desk x) = Desk x

-- 3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K a b = K a deriving (Eq, Show)

instance Functor (Flip K a) where
        fmap f (Flip (K a)) = Flip (K (f a))

-- 4

data EvilGoateeConst a b = GoatyConst b deriving (Show, Eq)

instance Functor (EvilGoateeConst a) where
        fmap f (GoatyConst b) = GoatyConst (f b)

-- 5

data LiftItOut f a = LiftItOut (f a)
                   deriving (Show, Eq)

instance Functor f => Functor (LiftItOut f) where
        fmap g (LiftItOut (x)) = LiftItOut (fmap g x)

-- 6
data Parappa f g a = DaWrappa (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap fn (DaWrappa x y) = DaWrappa (fmap fn x) (fmap fn y)

-- 7
data IgnoreOne f g a b= IgnoringSomething (f a) (g b) deriving (Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap fn (IgnoringSomething x y) = IgnoringSomething x (fmap fn y)

-- 8
data Notorius g o a t = Notorius (g o) (g a) (g t) deriving (Show)

instance Functor g => Functor (Notorius g o a) where
        fmap f (Notorius x y z) = Notorius x y (fmap f z)

-- 9
data List a = Nil
            | Cons a (List a) deriving (Show)

instance Functor List where
        fmap _ Nil = Nil
        fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving (Show)

instance Functor GoatLord where
        fmap _ NoGoat = NoGoat
        fmap f (OneGoat x) = OneGoat (f x)
        fmap f (MoreGoats x y z) = MoreGoats (fm x) (fm y) (fm z)
            where fm = fmap f

-- 11
data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)
                deriving (Show)

execRead :: TalkToMe a -> String -> Maybe a
execRead (Read f) str = Just (f str)
execRead _ _ = Nothing

instance Show (String -> a) where
        show f = "(String -> a) Function"

instance Functor TalkToMe where
        fmap _ Halt = Halt
        fmap f (Print s x) = Print s (f x)
        fmap f (Read g) = Read (fmap f g)

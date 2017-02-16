module Compose where

data Compose f m a = Compose { runCompose :: f (m a) } deriving (Show)

instance (Functor f, Functor m) => Functor (Compose f m) where
    fmap f (Compose fma) = Compose $ (fmap . fmap) f fma

instance (Applicative f, Applicative m) => Applicative (Compose f m) where
    pure x = Compose $ pure $ pure x
    
    Compose fmab <*> Compose fma = Compose $ fmap (<*>) fmab <*> fma


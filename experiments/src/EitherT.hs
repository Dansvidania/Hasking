module EitherT where

import Data.Monoid ((<>))
import Control.Monad.Trans.Class

newtype EitherT e m a = EitherT { runEitherT :: m ( Either e a ) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT ea) = EitherT $ (fmap . fmap) f ea

instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT $ pure $ pure x

    EitherT x <*> EitherT y = EitherT $ fmap (<*>) x <*> y

instance Monad m => Monad (EitherT e m) where
    return = pure
    
    EitherT x >>= f = EitherT $ do
                                  y <- x
                                  case y of 
                                    Right r -> runEitherT (f r)
                                    Left x -> return $ Left x


swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT amb) = do
                              eab <- amb
                              case eab of
                                Left a -> f a
                                Right e -> g e
                    

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

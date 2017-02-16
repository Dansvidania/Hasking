module StateT where

import Control.Monad
import Control.Monad.Trans.Class

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m, Monad m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ \s -> do
                                          (a, s1) <- sma s
                                          return (f a, s1)


instance (Monad m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  StateT f <*> StateT x = StateT $ \s -> do 
                                         (ax, sx) <- x s
                                         (ff, sf) <- f sx
                                         return (ff ax, sf)

instance (Monad m) => Monad (StateT s m) where
  return = pure

  sMas >>= sMbs = StateT $ \s -> runStateT ( sMas >>= sMbs ) s

instance MonadTrans (StateT s) where
  lift = StateT . (\ma s -> ma >>= \a -> return (a, s) )



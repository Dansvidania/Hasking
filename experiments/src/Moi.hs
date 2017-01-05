module Moi where

import Control.Applicative
import Control.Monad

newtype Moi s a = Moi
  { runMoi :: s -> (a, s)
  }

instance Functor (Moi s) where
  fmap f (Moi g) =
    Moi $
    \s ->
       let (a, st) = g s
       in (f a, st)

instance Applicative (Moi s) where
  pure f = Moi $ \s -> (f, s)
  (Moi f) <*> (Moi x) =
    Moi $
    \s ->
       let (ax, sx) = x s
           (ff, sf) = f sx
       in (ff ax, sf)

instance Monad (Moi s) where
  return = pure
  (Moi f) >>= g =
    Moi $
    \s ->
       let (a, sf) = f s
       in runMoi (g a) sf

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

exec :: Moi s a -> s -> s
exec (Moi sToA) s =
  let (_, sa) = sToA s
  in sa

eval :: Moi s a -> s -> a
eval (Moi sToA) s =
  let (a, _) = sToA s
  in a

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)

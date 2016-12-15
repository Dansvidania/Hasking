import Control.Monad
import Control.Applicative

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = liftA2 (:) (f x) (meh xs f)


onlyNatural :: Int -> Maybe Int
onlyNatural x
    | x >= 0 = Just x
    | otherwise = Nothing

flipType :: Monad m => [m a] -> m [a]
flipType ms = meh ms id

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x < 0 = Nothing
    | otherwise = Just (posToNat x)
    where posToNat 0 = Zero
          posToNat x = Succ (posToNat (x-1))


isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing = not . isJust

mayybee :: b -> (a->b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee b f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Nothing):ms) = catMaybes ms
catMaybes ((Just x):ms) = x : catMaybes ms

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe ms
    | any (isNothing) (ms) = Nothing
    | otherwise = Just ( catMaybes ms)

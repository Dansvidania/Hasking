import Data.List (elemIndex)

aMap = zip [1,2,3][4,5,6]


-- 1
added :: Maybe Integer
added = fmap (+3) $ (lookup 3 $ aMap)

-- 2
y :: Maybe Integer
y = lookup 3 $ aMap

z :: Maybe Integer
z = lookup 2 $ aMap

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3
x :: Maybe Int
x = elemIndex 3 [1..5]

w :: Maybe Int
w = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> w

-- 4
xs = [1,2,3]
ys = [4,5,6]

i :: Maybe Integer
i = lookup 3 $ aMap

j :: Maybe Integer
j = lookup 2 $ aMap

summed :: Maybe Integer
summed = fmap sum $ (,) <$> i <*> j

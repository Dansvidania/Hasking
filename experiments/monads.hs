
twoWhenEven :: [Integer] -> [Integer]
twoWhenEven xs = do
        x <- xs
        if even x
            then [x*x, x*x]
            else [x*x]

twoWhenEven' :: [Integer] -> [Integer]
twoWhenEven' xs = do
        x <- xs
        if even x
            then [x*x,x*x]
            else []

(%) :: Integral a => a -> a -> a
(%) = mod

fizzBuzz :: [Int] -> [String]
fizzBuzz xs = do
        x <- xs
        intToFizzBuzz x


intToFizzBuzz :: Int -> [String]
intToFizzBuzz x 
    | x % 15 == 0 = ["fizzbuzz"]
    | x % 3 == 0 = ["fizz"]
    | x % 5 == 0 = ["buzz"]
    | otherwise = [show x]




import Control.Applicative (liftA2)
import Data.Monoid ( (<>) )
import Data.Bool (bool)

(<!>) :: Monoid b => (a -> b) -> (a -> b) -> a -> b
(<!>) = liftA2 (<>)

fizzes :: Int -> Bool
fizzes x = x `mod` 3 == 0

buzzes :: Int -> Bool
buzzes x = x `mod` 5 == 0

printIf :: (Int -> Bool) -> String -> Int -> String
printIf f s x= bool "" s (f x)

fizz :: Int -> String
fizz =  printIf fizzes "fizz"

buzz :: Int -> String
buzz =  printIf buzzes "buzz"

showInt :: Int -> String
showInt x
    | fizzes x || buzzes x = ""
    | otherwise = show x

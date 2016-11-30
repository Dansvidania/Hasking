module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "1 + 1 is equal to 2" $ do
            (1 + 1) `shouldBe` 2
        it "1 + 1 is not equal to -2" $ do
            (1 + 1) == (-2) `shouldBe` False
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
    describe "Multiplication" $ do
        it "1 times 2 equals 2" $ do
            (1 `times` 2) `shouldBe` 2
        it "2 times 2 equals 4" $ do
            (2 `times` 2) `shouldBe` 4
        it "x times 2 divided by 2 = x" $ do
            property $ \x -> (x `times` 2) / 2 == (x :: Double)

times :: (Eq a, Num a) => a -> a -> a
_ `times` 0 = 0
x `times` y = x + (times x (y-1))
            
sayHello :: IO ()
sayHello = putStrLn "Hello"

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
        a <- arbitrary
        b <- arbitrary
        return (a, b)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
        a <- arbitrary
        b <- arbitrary
        elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
        a <- arbitrary
        elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
        a <- arbitrary
        frequency [(1, return Nothing)
                  , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

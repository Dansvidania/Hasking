import Test.QuickCheck
import Optional
import MonoidLaws

newtype First' a = First' { getFirst' :: Optional a }
                   deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
        arbitrary = do
            a <- arbitrary
            frequency $ [(1, return Nada), (3, return (First' a))]

instance Monoid (First' a) where
        mempty = First' Nada
        mappend mempty snd = snd
        mappend (First' (Only x)) _ = First' (Only x)



firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String
                 -> First' String
                 -> First' String
                 -> Bool

type FstId = First' String -> Bool



main :: IO ()
main = do 
          quickCheck (monoidAssoc :: FirstMappend)
          quickCheck (monoidLeftIdentity :: FstId)
          quickCheck (monoidRightIdentity :: FstId)

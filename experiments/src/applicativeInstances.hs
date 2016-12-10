newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
        fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
        pure = Identity
        (Identity x) <*> (Identity y) = Identity (x y)

newtype Constant a b = Constant { getConstant :: a }
                       deriving (Eq, Ord, Show)

instance Functor (Constant a) where
        fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
        pure x= Constant mempty
        (Constant x) <*> (Constant y) = Constant (x `mappend` y)



validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen then Nothing
                                                 else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a


data Person = Person Name Address
            deriving (Show, Eq)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a


data Cow = Cow {
         name   :: String
       , age    :: Int
       , weight :: Int
       } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing


cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString n a w = Cow <$> noEmpty n <*> noNegative a <*> noNegative w

import           Data.Monoid              (Monoid, (<>))
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
        arbitrary = Identity <$> arbitrary

instance Functor Identity where
        fmap g (Identity x) = Identity (g x)

instance Applicative Identity where
        pure = Identity
        Identity f <*> Identity x = Identity (f x)

instance Foldable Identity where
        foldMap f (Identity x) = f x

instance Monad Identity where
        return = pure
        Identity x >>= f = f x

instance Traversable Identity where
        traverse f (Identity x) = Identity <$> f x

instance Eq a => EqProp (Identity a) where
        (=-=) = eq


newtype Constant a b = Constant { getConstant :: a }
                       deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Constant a b) where
        arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
        (=-=) = eq

instance Functor (Constant a) where
        fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
        pure _ = Constant mempty
        (Constant x) <*> (Constant y) = Constant (x <> y)

instance Foldable (Constant a) where
        foldMap _ _ = mempty

instance Traversable (Constant a) where
        traverse _ (Constant x) = pure $ Constant x


data Optional a = Nada
                | Yep a deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Optional a) where
        arbitrary = frequency $ [ (1, return Nada)
                                , (3, Yep <$> arbitrary)
                                ]

instance Eq a => EqProp (Optional a) where
        (=-=) = eq

instance Monoid a => Monoid (Optional a) where
        mempty = Yep mempty
        mappend Nada Nada = Nada
        mappend Nada (Yep x) = Yep x
        mappend (Yep x) Nada = Yep x
        mappend (Yep x) (Yep y) = Yep (x <> y)

instance Functor Optional where
        fmap _ Nada    = Nada
        fmap f (Yep x) = Yep (f x)

instance Applicative Optional where
        pure = Yep
        Nada <*> _ = Nada
        _ <*> Nada = Nada
        Yep f <*> Yep x = Yep (f x)

instance Foldable Optional where
        foldMap _ Nada = mempty
        foldMap f (Yep x) = f x

instance Traversable Optional where
        traverse _ Nada = pure Nada
        traverse f (Yep x) = Yep <$> f x

data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance (Arbitrary a, Arbitrary b) =>
        Arbitrary (Pair a b) where
            arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
        (=-=) = eq

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
        mempty = Pair mempty mempty
        Pair x y `mappend` Pair a b = Pair (x <> a) (y <> b)

instance Functor (Pair a) where
        fmap f (Pair a x) = Pair a (f x)

instance Monoid a => Applicative (Pair a) where
        pure = Pair mempty
        (Pair x y) <*> (Pair a b) = Pair (x <> a) (y b)

instance Foldable (Pair a) where
        foldMap f (Pair _ x) = f x

instance Traversable (Pair a) where
        traverse f (Pair a x) = Pair a <$> f x


data S n a = S (n a) a deriving (Eq, Show, Ord)

instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
        arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
        (=-=) = eq

instance Functor n => Functor (S n) where
        fmap f (S m x) = S (fmap f m) (f x) 

instance Applicative n => Applicative (S n) where
        pure x = S (pure x) x
        S m x <*> S n y = S (m <*> n) (x y)

instance Foldable n => Foldable (S n) where 
        foldMap f (S (n) x) = foldMap f n <> f x

instance Traversable n => Traversable (S n) where
        traverse f (S (m) x) = S <$> (traverse f m) <*> f x


data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
            deriving (Eq, Ord, Show)

instance Functor Tree where
        fmap _ Empty = Empty
        fmap f (Leaf x) = Leaf (f x)
        fmap f (Node ls x ys) = Node (fmap f ls) (f x) (fmap f ys)

instance Foldable Tree where
        foldMap _ Empty = mempty
        foldMap f (Leaf x) = f x
        foldMap f (Node ls x ys) = 
                            (foldMap f ls) <> (f x) <> (foldMap f ys)

instance Traversable Tree where
        traverse _ Empty = pure Empty
        traverse f (Leaf x) = Leaf <$> f x
        traverse f (Node ls x ys) = Node <$> traverse f ls <*> f x <*> traverse f ys

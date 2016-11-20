-- Exercises for chapter 11 of Haskell Programming

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show


type Gardener = String

data Garden =
        Garden Gardener FlowerType
        deriving Show



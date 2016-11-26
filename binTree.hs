data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)


unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
                 Nothing -> Leaf
                 Just (w, y, z) -> Node (unfold f w) y (unfold f z)


treeBuild :: Integer -> BinaryTree Integer
treeBuild = unfold treeBuild' 
    where treeBuild' 0 = Nothing
          treeBuild' x = Just (x-1, x, x-1)

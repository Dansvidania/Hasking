splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn elem [] = []
splitOn elem str = 
        [takeWhile (predicate) str] ++ 
        ( splitOn (elem) $ ( drop 1 . dropWhile (predicate)) str )
        where predicate = (/= elem)


myWords :: String -> [String]
myWords str = splitOn ' ' str

myLines :: String -> [String]
myLines str = splitOn '\n' str

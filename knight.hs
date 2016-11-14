type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = let board = [1..8]
                       in [(x, y) | (x, y) <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
                  (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)], x `elem` board && y `elem` board]

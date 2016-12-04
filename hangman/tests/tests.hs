module Tests where
    
import Test.QuickCheck
import Hangman

instance Arbitrary Puzzle where
        arbitrary = puzzleGen

puzzleGen :: Gen Puzzle
puzzleGen = do
        str <- arbitrary
        return (freshPuzzle str)

testFillInChar = flip fillInCharacter

charIsInPuzzle :: Char -> Puzzle -> Bool
charIsInPuzzle c (Puzzle s showns guesses) = c `elem` guesses
                                        ||   (Just c) `elem` showns

checkPuzzle :: Char -> Puzzle -> Bool
checkPuzzle c p = charIsInPuzzle c $ testFillInChar c p 

runTests :: IO ()
runTests = do
        quickCheck checkPuzzle
        quickCheck checkPuzzle

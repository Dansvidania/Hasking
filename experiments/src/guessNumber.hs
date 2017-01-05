smallest = 0

biggest = 1000

guessNum
  :: Integral a
  => a -> a -> a
guessNum x y = y + (div (x - y) 2)

askIsBigger :: IO (Bool)
askIsBigger = do
  print $ "Is your number higher? (Y/N)"
  answer <- getLine
  return $
    case answer of
      "Y" -> True
      _ -> False

play :: Integer -> Integer -> IO ()
play x y = do
  print $ "is it " ++ (show $ guessNum x y) ++ "? (Y/N)"
  answer <- getLine
  case answer of
    "Y" -> print "Great! Victory!"
    _ -> do
      isBigger <- askIsBigger
      case isBigger of
        True -> play x (guessNum x y)
        False -> play (guessNum x y) y

main = do
  print $ "choose an Integer between 0 and 1000, but don't tell me!"
  play biggest smallest

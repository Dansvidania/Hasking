--Exercise for Haskell Programming at pag 555

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
        [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
        , DbNumber 9001
        , DbNumber 1001
        , DbString "Hello, world!"
        , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
        , DbDate (UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 1))
        ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map getTime . filter (isUTCTime)
    where isUTCTime (DbDate _ ) = True
          isUTCTime _ = False
          getTime (DbDate x) = x

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map getInteger . filter (isInteger)
    where isInteger (DbNumber _) = True
          isInteger _ = False
          getInteger (DbNumber x) = x

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x = (fromIntegral sumDB) / (fromIntegral freq)
    where sumDB = sumDb x
          freq = length $ filterDbNumber x 

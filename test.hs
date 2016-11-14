data Test = Test { recordInt :: Int
          , recordString :: String
          , recordBool :: Bool
          } deriving (Eq, Show, Read)

testItem = Test { recordInt = 1, recordString = "string", recordBool = True }

import Data.List

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlus
                     | Mac
                     | Windows
                     deriving (Eq, Show, Enum)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                         deriving (Eq, Show, Enum)

data Programmer = 
                Programmer { os :: OperatingSystem
                , lang :: ProgrammingLanguage }
                deriving (Eq, Show)

oss = [GnuPlusLinux .. ]
langs = [Haskell .. ]


allProgrammers :: [Programmer]
allProgrammers = [(Programmer { os = x, lang = h }) | x <- oss, h <- langs]

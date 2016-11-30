type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $ 
                            "Name was: " ++ show name ++
                            " Age was: " ++ show age

getName :: IO String
getName = do
        putStr "Please insert person name >"
        name <- getLine
        return name

getAge :: IO Integer
getAge = do
        putStr "Please insert person age >"
        age <- getLine
        return (read (age) :: Integer )


gimmePerson :: IO ()
gimmePerson = do
        name <- getName
        age <- getAge
        let optionPerson = mkPerson name age
        case optionPerson of
            Right (Person _ _) -> putStrLn $
                "Yay! Successfully got a person:" ++ show optionPerson
            Left NameEmpty -> putStrLn "Name cannot be empty!"
            Left AgeTooLow -> putStrLn "Age is too low!"
            Left (PersonInvalidUnknown x) -> putStrLn $ "Unknown error: " ++ x



type Label = Char
type Digit = Char

data Key = Key Label [Digit] 

type PhoneKeyboard = [Key]

keyboard :: PhoneKeyboard
keyBoard = [ Key '1' "1"
           , Key '2' "abc2"
           , Key '3' "def3"
           , Key '4' "ghi4"
           , Key '5' "jkl5"
           , Key '6' "mno6"
           , Key '7' "pqrs7"
           , Key '8' "tuv8"
           , Key '9' "wxyz9"
           , Key '0' " 0"
           , Key '*' []
           , Key '#' ".,"
           ]

digitToKeypress = 

convo :: [String]
convo = [ "Wanna play 20 questions"
        , "Ya"
        , "U 1st haha"
        , "Lol ok. Have u ever tasted alcohol lol"
        , "Lol ya"
        , "Lol ur cool haha. Ur turn"
        ]



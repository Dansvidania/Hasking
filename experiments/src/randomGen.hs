
import Test.QuickCheck

data Fool = Fulse
          | Frue
          deriving (Eq, Show)

eqFoolGen :: Gen Fool
eqFoolGen = elements $ [Fulse,Frue]

nonEqFoolGen :: Gen Fool
nonEqFoolGen = frequency $ [(2, return Fulse), (1, return Frue)]

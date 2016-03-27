type State = String
type M a = State -> [(a, State)]

data Term = Con Int
          | Div Term Term
          deriving (Show)

item :: M Char
item [] = []
item (a:x) = [(a, x)]

main :: IO()
main = do
    print $ item ""
    print $ item "monad"

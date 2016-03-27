type State = String
type M a = State -> [(a, State)]

data Term = Con Int
          | Div Term Term
          deriving (Show)

unit :: a -> M a
unit a = \x -> [(a, x)]

star :: M a -> (a -> M b) -> M b
m `star` k = \x -> [(b, z) | (a, y) <- m x, (b, z) <- k a y]

item :: M Char
item [] = []
item (a:x) = [(a, x)]

twoItems :: M (Char, Char)
twoItems = item `star` \a -> (item `star` \b -> unit(a, b))

main :: IO()
main = do
    print $ twoItems ""
    print $ twoItems "monad"

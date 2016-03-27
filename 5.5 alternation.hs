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

oplus :: M a -> M a -> M a
m `oplus` n = \x -> (m x) ++ (n x)

oneOrTwoItems :: M String
oneOrTwoItems = (item `star` \a -> unit [a]) `oplus`
                (item `star` \a -> (item `star` \b -> unit [a, b]))

main :: IO()
main = do
    print $ oneOrTwoItems ""
    print $ oneOrTwoItems "m"
    print $ oneOrTwoItems "monad"

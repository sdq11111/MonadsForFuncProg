import Data.Char

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

zero :: M a
zero x = []

oplus :: M a -> M a -> M a
m `oplus` n = \x -> (m x) ++ (n x)

ofilter :: M a -> (a -> Bool) -> M a
m `ofilter` p = m `star` \a -> if p a then unit a else zero

letter :: M Char
letter = item `ofilter` isLetter

digit :: M Int
digit = (item `ofilter` isDigit) `star` \a -> unit ((ord a) - (ord '0'))

lit :: Char -> M Char
lit c = item `ofilter` \a -> a == c

main :: IO()
main = do
    print $ lit 'm' "monad"
    print $ lit 'm' "parse"

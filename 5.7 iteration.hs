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

oiterate :: M a -> M [a]
oiterate m = (m `star` \a -> oiterate m `star` \x -> unit (a:x)) `oplus`
             unit []

asNumber :: [Int] -> Int
asNumber a = asNumber' a 0
    where asNumber' :: [Int] -> Int -> Int
          asNumber' []     x = x
          asNumber' (a:ax) x = asNumber' ax (x * 10 + a)

number :: M Int
number = digit `star` \a -> (oiterate digit `star` \x -> unit (asNumber (a:x)))

main :: IO()
main = do
    print $ oiterate digit "23 and more"
    print $ number "23 and more"

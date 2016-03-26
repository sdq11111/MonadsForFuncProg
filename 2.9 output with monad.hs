type Output = String
type M a = (Output, a)

unit :: a -> M a
unit a = ("", a)

star :: M a -> (a -> M b) -> M b
m `star` k = let (x, a) = m in
             let (y, b) = k a in
             (x ++ y, b)

out :: Output -> M ()
out x = (x, ())

line :: Term -> Int -> Output
line t a = "eval(" ++ (show t) ++ ") <= " ++ (show a) ++ "\n"

data Term = Con Int
          | Div Term Term
          deriving (Show)

eval :: Term -> M Int
eval (Con a) = out (line (Con a) a) `star` \() -> unit a
eval (Div t u) = (eval t) `star` \a -> 
                    ((eval u) `star` \b -> 
                      out (line (Div t u) (div a b)) `star` (\() -> (unit (div a b))))

answer, error_term :: Term
answer = (Div (Div (Con 1972) (Con 2))(Con 23))
error_term = (Div (Con 1) (Con 0))

main :: IO()
main = do
    putStrLn $ fst (eval answer)
    putStrLn $ fst (eval error_term)

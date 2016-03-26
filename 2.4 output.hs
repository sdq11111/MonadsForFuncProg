type Output = String
type M a = (Output, a)

data Term = Con Int
          | Div Term Term
          deriving (Show)

line :: Term -> Int -> Output
line t a = "eval(" ++ (show t) ++ ") <= " ++ (show a) ++ "\n"

eval :: Term -> M Int
eval (Con a) = (line (Con a) a, a)
eval (Div t u) = let (x, a) = eval t in
                 let (y, b) = eval u in
                 (x ++ y ++ (line (Div t u) (div a b)), div a b)

answer, error_term :: Term
answer = (Div (Div (Con 1972) (Con 2))(Con 23))
error_term = (Div (Con 1) (Con 0))

main :: IO()
main = do
    putStrLn $ fst (eval answer)
    putStrLn $ fst (eval error_term)

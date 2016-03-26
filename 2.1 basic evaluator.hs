data Term = Con Int
          | Div Term Term

eval :: Term -> Int
eval (Con a) = a
eval (Div t u) = div (eval t) (eval u)

answer, error_term :: Term
answer = (Div (Div (Con 1972) (Con 2))(Con 23))
error_term = (Div (Con 1) (Con 0))

main :: IO()
main = do
    print (eval answer)
    print (eval error_term)

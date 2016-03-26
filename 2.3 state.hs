type State = Int
type M a = State -> (a, State)

data Term = Con Int
          | Div Term Term

eval :: Term -> M Int
eval (Con a) x = (a, x)
eval (Div t u) x = let (a, y) = eval t x in
                   let (b, z) = eval u y in
                   (div a b, z + 1)

answer, error_term :: Term
answer = (Div (Div (Con 1972) (Con 2))(Con 23))
error_term = (Div (Con 1) (Con 0))

main :: IO()
main = do
    print (eval answer 0)
    print (eval error_term 0)
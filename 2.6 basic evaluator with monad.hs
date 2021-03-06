type M a = a

unit :: a -> M a
unit a = a

star :: M a -> (a -> M b) -> M b
a `star` k = k a

data Term = Con Int
          | Div Term Term
          deriving (Show)

eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = (eval t) `star` \a -> 
                    ((eval u) `star` \b -> 
                      (unit (div a b)))

answer, error_term :: Term
answer = (Div (Div (Con 1972) (Con 2))(Con 23))
error_term = (Div (Con 1) (Con 0))

main :: IO()
main = do
    print (eval answer)
    print (eval error_term)

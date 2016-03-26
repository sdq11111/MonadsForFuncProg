type State = Int
type M a = State -> (a, State)

unit :: a -> M a
unit a = \x -> (a, x)

star :: M a -> (a -> M b) -> M b
m `star` k = \x -> let (a, y) = m x in k a y

tick :: M ()
tick = \x -> ((), x + 1)

data Term = Con Int
          | Div Term Term
          deriving (Show)

eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = (eval t) `star` \a -> 
                    ((eval u) `star` \b -> 
                      tick `star` (\() -> (unit (div a b))))

answer, error_term :: Term
answer = (Div (Div (Con 1972) (Con 2))(Con 23))
error_term = (Div (Con 1) (Con 0))

main :: IO()
main = do
    print (eval answer 0)
    print (eval error_term 0)

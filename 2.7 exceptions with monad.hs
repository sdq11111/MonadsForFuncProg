import Control.Monad
import Control.Applicative

type Exception = String
data M a = Raise Exception
         | Return a
         deriving (Show)

unit :: a -> M a
unit a = Return a

star :: M a -> (a -> M b) -> M b
m `star` k = case m of
                Raise e -> Raise e
                Return a -> k a

raise :: Exception -> M a
raise e = Raise e

data Term = Con Int
          | Div Term Term
          deriving (Show)

eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = (eval t) `star` \a -> 
                    ((eval u) `star` \b -> 
                      if b == 0
                      then raise "divide by zero"
                      else (unit (div a b)))

answer, error_term :: Term
answer = (Div (Div (Con 1972) (Con 2))(Con 23))
error_term = (Div (Con 1) (Con 0))

main :: IO()
main = do
    print (eval answer)
    print (eval error_term)

import Control.Monad
import Control.Applicative

data M a = Unit a
         deriving (Show)

instance Monad M where
    (Unit a) >>= f = f a
    return = Unit

instance Functor M where
    fmap = liftM

instance Applicative M where
    pure = return
    (<*>) = ap

data Term = Con Int
          | Div Term Term
          deriving (Show)

eval :: Term -> M Int
eval (Con a) = return a
eval (Div t u) = (eval t) >>= \a -> ((eval u) >>= \b -> (return (div a b)))

answer, error_term :: Term
answer = (Div (Div (Con 1972) (Con 2))(Con 23))
error_term = (Div (Con 1) (Con 0))

main :: IO()
main = do
    print (eval answer)
    print (eval error_term)
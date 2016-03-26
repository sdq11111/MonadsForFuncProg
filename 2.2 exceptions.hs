type Exception = String

data M a = Raise Exception
         | Return a
         deriving (Show)

data Term = Con Int
          | Div Term Term

eval :: Term -> M Int
eval (Con a) = Return a
eval (Div t u) = case eval t of
                    Raise e -> Raise e
                    Return a ->
                        case eval u of
                            Raise e -> Raise e
                            Return b ->
                                if b == 0
                                    then Raise "Divide by zero"
                                    else Return (div a b)

answer, error_term :: Term
answer = (Div (Div (Con 1972) (Con 2))(Con 23))
error_term = (Div (Con 1) (Con 0))

main :: IO()
main = do
    print (eval answer)
    print (eval error_term)

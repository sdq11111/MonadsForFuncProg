type Id = String
type Ix = Id
type Val = Int

data Arr = Head Val Arr
         | Elem Ix Val Arr Arr
         | None
         deriving (Show)

newarray :: Val -> Arr
newarray v = Head v None

index :: Ix -> Arr -> Val
index i (Head def x) = index' i x
    where index' :: Ix -> Arr -> Val
          index' i (Elem idx val l r) | i == idx  = val
                                      | i < idx   = index' i l
                                      | otherwise = index' i r
          index' i None = def

update :: Ix -> Val -> Arr -> Arr
update i v (Head def x) = Head def (update i v x)
update i v (Elem idx val l r) | i == idx  = Elem idx v l r
                              | i < idx   = Elem idx val (update i v l) r
                              | otherwise = Elem idx val l (update i v r)
update i v None = Elem i v None None

data Term = Var Id
          | Con Int
          | Add Term Term
          deriving (Show)

data Comm = Asgn Id Term
          | Seq Comm Comm
          | If Term Comm Comm
          deriving (Show)

data Prog = Prog Comm Term
          deriving (Show)

type State = Arr
           deriving (Show)

eval :: Term -> State -> Int
eval (Var i) x = index i x
eval (Con a) x = a
eval (Add t u) x = (eval t x) + (eval u x)

exec :: Comm -> State -> State
exec (Asgn i t) x = update i (eval t x) x
exec (Seq c d) x = exec d (exec c x)
exec (If t c d) x = if (eval t x) == 0 then exec c x else exec d x

elab :: Prog -> Int
elab (Prog c t) = eval t (exec c (newarray 0))

main :: IO()
main = do
    print $ elab ((Prog (Asgn "x" (Con 4)) (Add (Con 5) (Var "x"))))

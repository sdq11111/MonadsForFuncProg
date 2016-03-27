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

type M a = State -> (a, State)

unit :: a -> M a
unit a = \x -> (a, x)

star :: M a -> (a -> M b) -> M b
m `star` k = \x -> let (a, y) = m x in k a y

block :: Val -> M a -> a
block v m = let (a, x) = m (newarray v) in a

assign :: Ix -> Val -> M()
assign i v = \x -> ((), update i v x)

type M' a = State -> a

unit' :: a -> M' a
unit' a = \x -> a

star' :: M' a -> (a -> M' b) -> M' b
m `star'` k = \x -> let a = m x in k a x

fetch' :: Ix -> M' Val
fetch' i = \x -> index i x

coerce :: M' a -> M a
coerce m = \x -> let a = m x in (a, x)

eval :: Term -> M' Int
eval (Var i) = fetch' i
eval (Con a) = unit' a
eval (Add t u) = (eval t) `star'` \a -> ((eval u) `star'` \b -> unit' (a + b))

exec :: Comm -> M ()
exec (Asgn i t) = (coerce (eval t)) `star` \a -> assign i a
exec (Seq c d) = (exec c) `star` (\() -> (exec d) `star` \() -> unit ())
exec (If t c d) = (coerce (eval t)) `star` \a -> if a == 0 then exec c else exec d

elab :: Prog -> Int
elab (Prog c t) = block 0 ((exec c) `star` \() -> (coerce (eval t)) `star` \a -> unit a)

main :: IO()
main = do
    print $ elab ((Prog 
      (Seq (Asgn "x" (Con 4)) 
           (If (Con 2) 
               (Asgn "y" (Con 4)) 
               (Asgn "y" (Con 5)))) 
      (Add (Var "y") (Var "x"))))

unit :: a -> [a]
unit a = [a]

star :: [a] -> (a -> [b]) -> [b]
[]    `star` _ = []
(a:x) `star` k = (k a) ++ (x `star` k)

main :: IO()
main = print $ [1, 2, 3] `star` \x -> unit (x * x)

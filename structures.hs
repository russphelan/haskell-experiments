factorial n = factorial' n 1
  where factorial' 0 prod = prod
        factorial' n prod = factorial' (n - 1) (prod * n)

area r = let pi' = 3.14159
         in pi' * r^2

foldr' :: (b->a->a)->a->[b]->a
foldr' f acc (x:xs) = x `f` (foldr' f acc xs)
foldr' _ acc [] = acc

foldl' :: (a->b->a)->a->[b]->a
foldl' f acc (x:xs) = (foldl' f acc xs) `f` x
foldl' _ acc [] = acc

data Point = P Int Int deriving Show

x :: Point->Int
x (P first _) = first

y :: Point->Int
y (P _ second) = second

data Direction = Up | Left | Down | Right deriving Show

move :: Direction->Point->Point
move Up (P x y) = P x (y + 1)
move Left (P x y) = P (x - 1) y
move Down (P x y) = P x (y - 1)
move Right (P x y) = P (x + 1) y

type Path = [Direction]

follow :: Path->Point->Point
follow path point = foldl' (flip move) point path

data List a = L a (List a) | Empty

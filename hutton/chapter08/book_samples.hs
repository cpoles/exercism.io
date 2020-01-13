module BookSamples where


type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k',v) <- t, k == k']
        

type Pos = (Int, Int)

data Move = North | South | East | West deriving Show

move :: Move -> Pos -> Pos
move North (x,y) = (x, y + 1)
move South (x,y) = (x, y - 1)     
move East  (x,y) = (x + 1, y)
move West  (x,y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p     = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East



data Shape = Circle Float | Rect Float Float deriving Show

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y 

-- Using type Maybe for safe versions of div and safehead

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safehead :: [a] -> Maybe a
safehead []     = Nothing
safehead (x:_) = Just x

--- Recursive types

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) | x == y    = True
                      | x < y     = occurs x l
                      | otherwise = occurs x r     

flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

rmdups :: Ord a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) | x `elem` xs = rmdups xs
              | otherwise = x : rmdups xs

-- Abstract Machine
data Expr = Val Int | Add Expr Expr

--value :: Expr -> Int
-- value (Val n)   = n
-- value (Add x y) = value x + value y
type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int ->  Int
exec []           n = n 
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n + m)

-- redefining value using eval

value :: Expr -> Int
value e = eval e []

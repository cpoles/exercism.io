module Chapter08Ex where

-- Ex 1. Define a recursive multiplication function
data Nat = Zero | Succ Nat deriving Show

-- function Add
add :: Nat -> Nat -> Nat
add Zero     n = n 
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _        = Zero
mult m _           = add m (add m Zero)

-- Ex.2 Redefine the function occurs using the function compare
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case x `compare` y of
                          EQ -> True
                          LT -> occurs x l
                          GT -> occurs x r

-- Ex.3 Define a function balanced :: Tree a -> Bool
data NewTree a = NewLeaf a | NewNode (NewTree a) (NewTree a) deriving (Show)

treeSize :: NewTree a -> Integer
treeSize (NewLeaf _)   = 1
treeSize (NewNode l r) = treeSize l + treeSize r 

balanced :: NewTree a -> Bool
balanced (NewLeaf _)   = True
balanced (NewNode l r) = abs (treeSize l - treeSize r) <= 1
                         && balanced l && balanced r

-- Ex.4 Define a function balance :: [a] -> Tree a
halves :: [a] -> ([a], [a])
halves xs = splitAt half xs
            where half = length xs `div` 2

balance :: [a] -> NewTree a
balance [x]    = NewLeaf x
balance xs     = NewNode (balance fstHalf) (balance sndHalf)
             where (fstHalf, sndHalf) = halves xs      

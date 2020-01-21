module Chapter12Ex where

-- Ex 1. Define an instance of Functor for the class below
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
-- fmap :: (a -> b) -> Tree a -> Tree b
   fmap _ (Leaf)       = Leaf
   fmap g (Node l y r) = Node (fmap g l) (g y) (fmap g r)

tree = Node
           (Node (Leaf) 3 (Node
                              (Node (Leaf) 7 (Leaf)) 8
                              (Node (Leaf) 9 (Leaf)))) 1
           (Node (Leaf) 5 (Node
                              (Node (Leaf) 11 (Leaf)) 12
                              (Node (Leaf) 13 (Leaf)))) 

-- Ex 2. Complete the instance declaration to make the partially applied function type (a -> ) into a functor:


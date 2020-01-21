module BookExamples where

-- Functors

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
   -- fmap :: (a -> b) -> Tree a -> Tree b
   fmap g (Leaf x)   = Leaf (g x)
   fmap g (Node l r) = Node (fmap g l) (fmap g r)


tree :: Tree Int 
tree = Node
           (Leaf 1) (Node
                         (Node (Leaf 3) (Node (Leaf 5) (Leaf 6)))
                         (Node (Leaf 4) (Node (Leaf 7) (Leaf 8))))
                               

-- generalised version of Increment using Functor
inc :: Functor f => f Int -> f Int
inc = fmap (+1)

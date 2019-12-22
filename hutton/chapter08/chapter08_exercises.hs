module Chapter08Ex where

-- Ex 1. Define a recursive multiplication function
data Nat = Zero | Succ Nat deriving Show

-- function Add
add :: Nat -> Nat -> Nat
add Zero     n = n 
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _        = Zero
mult (Succ Zero) n = n
mult m _           = add m (add m Zero)

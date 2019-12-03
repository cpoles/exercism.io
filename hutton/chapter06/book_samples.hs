module BookSamples where

import Prelude hiding (length, product, reverse, even, odd)

-- factorial function
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

-- function product
product :: Num a => [a] -> a
product []     = 1
product (n:ns) = n * product ns 

-- function length
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

-- function reverse
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- function insert
insert :: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys 

-- function isort
isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- mutual recursion
even :: Int -> Bool
even 0 = True
even n = odd (n - 1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n - 1)

evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds []      = []
odds (_:xs)  = evens xs


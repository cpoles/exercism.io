module Chapter05 where

import Prelude hiding (replicate)
import Data.Char
-- 1. Sum of the first 100 integer squares

firstOneHundred = [x^2 | x <- [1..100]]

-- 2.  Coordinate Grid

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 3. Function square. Excludes the coordinates of the diagonal of the grid

square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y ]

-- 4. Define the library function replicate using list comprehension
replicate :: Int -> a -> [a]
replicate n x = [x | _ <- [1..n]] 

-- 5. Function that return triples of Pythagorean integers

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6. Function that returns all perfect numbers up to n
factors n = [x | x <- [1..n], n `mod` x == 0]

isPerfect :: Int -> Bool
isPerfect n = sum (init (factors n)) == n

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], isPerfect x]

-- 7. Using individual generators and the function concat, redefine the list comprehension
-- [(x,y) | x <- [1,2], y <- [3,4]]
--concat [[(x,y) | x <- [1,2]] | y <- [3,4]]



-- 8. Redefine the function positions using the function find
--
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- 9. Scalar Product of two lists

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys] 

-- 10. Modify the Caesar cipher to also handle upper-case letters

let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | isUpper c = ord c - ord 'A'
          | otherwise = ord c

int2let :: Int -> Char
int2let n = chr n

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26 + ord 'a')
          | isUpper c = int2let ((let2int c + n) `mod` 26 + ord 'A')
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

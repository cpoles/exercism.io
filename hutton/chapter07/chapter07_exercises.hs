module Chapter08Exercises where


import Prelude hiding (all, any, takeWhile, dropWhile, map, filter, curry, uncurry, iterate)
-- Ex.1
-- define a function using map and filter to re-express
-- [f x | x <- xs, p x]

mapAndFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapAndFilter f p = map f . filter p

-- Ex.2
-- define the following higher-order functions from the standard prelude

-- a. Decide if all elements of a list satisfy a predicate:
all :: (a -> Bool) -> [a] -> Bool
all _ []      = True
all p (x:xs)  = case p x of
                  True  -> all p xs
                  _     -> False  

-- b. Decide if any element of a list satisfies a predicate:
any :: (a -> Bool) -> [a] -> Bool
any _ []      = False
any p (x:xs)  = case p x of
                  False -> any p xs
                  _     -> True

-- c. Select elements from a list while they satisfy a predicate:
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs) | p x = x : takeWhile p xs
                   | otherwise = takeWhile p xs 

-- d. Remove elements from a list while they satisfy a predicate
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs) | p x = dropWhile p xs 
                   | otherwise = (x:xs) 

-- Ex.3
-- Redefine the functions map f and filter p using foldr.
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x:xs else xs) []

-- Ex.4
-- Using foldl, define a function dec2int :: [Int] -> Int that converts a decimal number into an integer
dec2int :: [Int] -> Int
dec2int = foldl (\y x -> x + 10 * y) 0

-- Ex.5
-- define the higher-order functions curry and uncurry
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y

-- Ex.6
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)
-- Redefine chop8, map f and iterate f using unfold
type Bit = Int

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (null) (take 8) (drop 8)

mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold f = unfold (null) (f . head) (tail) 

iterate :: (Ord a, Num a) => (a -> a) -> a -> [a]
iterate f = unfold (==0) f f

-- Ex. 7 and Ex.8
-- Refer to binary string transmitter file.

-- Ex. 9
-- Define altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- applies its argument function to successive elements in a list
-- altMap (+10) (+100) [0,1,2,3,4] -> [10,101,12,103,14]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []     = [] 
altMap f g (x:xs) = f x : altMap g f xs

-- Ex. 10
-- implement the Luhn algorithm using altMap. 
luhnDouble :: Int -> Int
luhnDouble x = 2 * x

greaterThanNine :: Int -> Int
greaterThanNine x = if x > 9 then x - 9 else x

luhn :: [Int] -> Bool 
luhn []  = False
luhn [_] = False
luhn xs  = sum (altMap (*1) (greaterThanNine . luhnDouble) (reverse xs)) `mod` 10 == 0




















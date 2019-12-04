module Chapter06Exercises where
-- Ex. 1
-- Modify factorial to accept only positive integers

fac :: Int -> Int
fac 0         = 1
fac n | n > 0 = n * fac (n - 1)
      | otherwise = error "n must be a positive number."

-- Ex. 2
-- Define a function that returns the sum of the non-negative integers from agiven value down to zero.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- Ex.3
-- Define the exponentiation operator ^ for non-negative integers using the ssame pattern of recursion as the multiplication operator.

myExp :: Int -> Int -> Int
myExp _ 0 = 1
myExp n e = n * myExp n (e - 1) 

-- Eval. 3^2
-- myExp 3 2 
-- 3 * (myExp 3 1)
-- 3 * (3 * myExp 3 0)
-- 3 * (3 * 1) = 3 * 3 = 9

-- Ex.4
-- Define a recursive function that implements the Euclid's algorithm for gcd
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y  = euclid x (y - x)
           | x > y  = euclid (x - y) y 

-- Ex.6
-- Using list recursing define the library functions 
-- and :: [Bool] -> Bool
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) | x == True = myAnd xs
             | otherwise = False

-- concat :: [[a]] -> [a]
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss) = xs ++ myConcat xss  

-- replicate :: Int -> a -> [a]
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n - 1) x

-- (!!) :: [a] -> Int -> a
myIndex :: [a] -> Int -> a
myIndex [] _      = error "Index too large"
myIndex (x:_) 0   = x
myIndex (_:xs) n  = myIndex xs (n - 1)

-- elem :: Eq a => a -> [a] -> Bool
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem e (x:xs) | e == x = True
                | otherwise = myElem e xs

-- Ex. 7
-- Define a recursive function merge :: Ord a=> [a] -> [a] -> [a]
-- that merges two sorted lists to give a single sorte list
merge :: Ord a => [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys) 
                    | otherwise  = y : merge (x:xs) ys

-- Ex 8. 
-- Using merge define a function msort :: Ord a => [a] -> [a]

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
            where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
                  where (ys, zs) = halve xs

-- Ex.9 Construct library function that
-- a. calculate the sum of a list of numbers
mySum :: Num a => [a] -> a
mySum []     = 0
mySum (x:xs) = x + mySum xs

-- b. take a given number of elements for the start of a list
myTake :: Int -> [a] -> [a]
myTake _ []     = []
myTake 0 _      = []
myTake n (x:xs) = x : myTake (n - 1) xs

-- c. select the last element of a non-empty list
selectLast :: [a] -> a
selectLast []     = error "list cannot be empty"
selectLast [x]    = x
selectLast (_:xs) = selectLast xs



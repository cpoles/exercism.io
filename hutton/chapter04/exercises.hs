module Chapter04 where

-- 1. Using library functions, define a function halve:: [a] -> ([a],[a])
-- that splits an even-lengthed list into two halves

halve :: [a] -> ([a], [a])
halve xs = (firstHalf, secondHalf)
             where
              firstHalf = take (length xs `div` 2) xs
              secondHalf = drop (length xs `div` 2) xs

-- 2. Define a function third :: [a] -> a that returns the third element in a list
-- that contains at least this many elements using:
-- a. head and tail
-- b. list indexing !!
-- pattern matching
third :: [a] -> a

-- a.
third (_:xs) = head (tail xs)
-- b.
third xs = xs !! 3
-- c.
third (_:_:x:_) = x

-- 3. Consider a function safetail :: [a] -> [a] that behaves in the same way as tail
-- except that it maps the empty list to itself rather than producing an error.
-- Using tail and the function null ;: [a] -> Bool that decides if a list is empty or not,
-- define safetail using:
safetail :: [a] -> [a]
-- a. a conditional expression
safetail xs = if null xs then [] else tail xs
-- b. guarded equations
safetail xs | null xs = []
            | otherwise = tail xs
-- c. pattern matching.
safetail [] = []
safetail (_:xs) = xs

-- 4. In a similar way to && in section 4.4, show how the disjunction operator || can be defined in four different ways using pattern matching
--
myOR :: Bool -> Bool -> Bool
myOR False False = False
myOR _ _         = True

myOR False False = True
myOR False True  = True
myOR True False  = True
myOR True True   = True

myOR b c | b == True = True
          | c == True = True
          | otherwise = False

myOR x y = if x 
            then True
            else if not y
              then False
              else True

-- 5. Without using any other library functions or operators, show how the meaning of the
-- following pattern matching definition for logical conjunction && can be formalised using
-- conditional expressions:
--
-- True && True = True
-- _    && _    = False
altAnd :: Bool -> Bool -> Bool 
altAnd x y = if not x
              then False
              else if y
               then True
               else False


-- 6. Do the same for the following alternative definition, and note that the difference
-- in the numer of conditional expressions that are required
-- True && b = b
-- False && _ = False

altAnd x y = if x
              then y
              else False




-- 7. Show how the menaing of the following curried function definition can be formalised in terms of lambda expressions:
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z


-- 8. The Luhn Algorithm

luhnDouble :: Int -> Int
luhnDouble n | 2 * n > 9 = 2 * n - 9
             | otherwise = 2 * n

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + luhnDouble b + luhnDouble c + d) `mod` 10 == 0 

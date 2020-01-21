module Chapter10Ex where

import Data.Char

-- 1. Redefine putStr :: String -> IO () using a list comprehension nad a the library function sequence

myPutStr :: String -> IO ()
myPutStr xs = sequence_ [putChar x | x <- xs]

-- 2. Using recursion, define a version of putBoard :: Board -> IO () that display nim boards of any size.
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard xs = putBoard' 1 xs 

putBoard' :: Int -> Board -> IO ()
putBoard' _ []     = return () 
putBoard' n (x:xs) = do putRow n x
                        putBoard' (n+1) xs 

-- 3. Redefine the generalised version of putBoard using a list comprehension and sequence_
putBoard'' :: Board -> IO ()
putBoard'' xs = sequence_ [putRow x y | (x,y) <- zip [1..] xs]

-- 4. Define an action adder :: IO () that reads a given number of integers from the keyboard, one per line, and displays their sum

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getDigit
           digits <- numbers n []
           putStrLn ("The total is " ++ show (sum digits))

getDigit :: IO Int
getDigit = do x <- getChar
              putStrLn ""
              if isDigit x then
                 return (digitToInt x)
              else
                 do putStrLn "Error."
                    getDigit

numbers :: Int -> [Int] -> IO [Int]
numbers 0 _ = return []
numbers n xs = do
                 x <- getDigit
                 ys <- numbers (n-1) (x:xs)
                 return (x:ys) 

-- 5. Redefine adder using the function sequence
adder' :: IO ()
adder' = do putStr "how many numbers? "
            n <- getDigit
            xs <- sequence [getDigit | _ <- [1..n]]
            putStrLn ("The total is " ++ show (sum xs))

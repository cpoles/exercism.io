module VotingAlgorithms where

import Data.List

-- FIRST PAST THE POST SYSTEM

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- function that counts the number of occurrences of x in a given list
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- Function that removes duplicates
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

-- function that returns the result of first-past-post-election in increasing order of the number of votes received
result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs] 

-- function that decide the election winner
winner :: Ord a => [a] -> a
winner = snd. last . result

-- ALTERNATIVE VOTE
ballots :: [[String]]
ballots = [["Red", "Green"],
           ["Blue"],
           ["Green", "Red", "Blue"],
           ["Blue", "Green", "Red"],
           ["Green"]]

-- function that removes empty ballots
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

-- function that eliminates a given candidate from each ballot
elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

-- function that ranks the 1st-choice candidates in each ballot in increasing order of the number of such vots
-- that were received.
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- function that decides the winner
winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
               [c]    -> c
               (c:cs) -> winner' (elim c bs)

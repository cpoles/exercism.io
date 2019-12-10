module BinaryTransmitter where

import Data.Char

type Bit = Int

--	BASE CONVERSION 

-- function that converts from binary to int using foldr
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0


-- function that converts from decimal to binary
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- function that truncates or extends a binary number to make it 8 bits
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- function that counts the number of ones in a 8-bit binary number
countOnes :: [Bit] -> Int
countOnes xs = foldr (+) 0 (filter (==1) xs)

-- function that adds the ParityBit to the 8-bit binary number
addParityBit :: [Bit] -> [Bit]
addParityBit xs | odd $ countOnes xs  = xs ++ [1]
                | otherwise = xs ++ [0]

-- TRANSMISSION
-- function that encodes a string of chars as a list of bits 
encode :: String -> [Bit]
encode = concat . map (addParityBit . make8 . int2bin . ord)

-- function that decodes a list produced by encode - chops a list of into 9-bit binary numbers
chop9 :: [Bit] -> [[Bit]]
chop9 []     = []
chop9 bits   = take 9 bits : chop9 (drop 9 bits)

-- function that checks the parity bit, removes it and returns 8-bit binary number
checkParityBit :: [Bit] -> [Bit] 
checkParityBit xs = if odd $ countOnes (init xs)
                      then if last xs == 1
                           then init xs 
                           else error "ParityBit error."
                      else if last xs == 0
                           then init xs
                           else error "ParityBit error."


-- function that decodes a list of bits as a string of chars by chopping the list up
decode :: [Bit] -> String
decode = map (chr . bin2int . checkParityBit) . chop9

-- function that simulates the transmission of a string of chars as a list of bits
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

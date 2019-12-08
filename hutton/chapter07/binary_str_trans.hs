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

-- TRANSMISSION
-- function that encodes a string of chars as a list of bits 
encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

-- function that decodes a list produced by encode - chops a list of into eight-bit binary numbers
chop8 :: [Bit] -> [[Bit]]
chop8 []     = []
chop8 bits   = take 8 bits : chop8 (drop 8 bits)

-- function that decodes a list of bits as a string of chars by chopping the list up
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

-- function that simulates the transmission of a string of chars as a list of bits
transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

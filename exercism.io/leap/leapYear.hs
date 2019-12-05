module LeapYear where

isDivisible :: Int -> Int -> Bool
isDivisible x y = x `mod` y == 0

isLeapYear :: Int -> Bool
isLeapYear year
  | isDivisible year 400  = True
  | isDivisible year 100  = False
  | isDivisible year 4    = True
  | otherwise             = False

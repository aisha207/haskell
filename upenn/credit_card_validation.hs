{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0 = toDigits (n `div` 10) ++ [(n `mod` 10)]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = []

-- From the right every second number doubled e.g.
-- doubleEveryOther [8,7,6,5] == [16,7,12,5]
-- doubleEveryOther [1,2,3] == [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:[])) = 2 * x : y : []
doubleEveryOther (x:(y:(z:[]))) = x : 2 * y : z :[]
doubleEveryOther (x:(y:zs)) = 2 * x : y : doubleEveryOther zs

-- Sum the digits e.g. [16, 2] == 1 + 6 + 2
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- Validate a credit card number
validateCardNumber :: Integer -> Bool
validateCardNumber n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

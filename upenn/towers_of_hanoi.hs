{-# OPTIONS_GHC -Wall #-}

type Peg = String
type Move = (Peg, Peg)

-- hanoi has a simple recuresive logic (to go from a to b):
-- move top n-1 discs from a to c using b as spare
-- move bottom n disc from a to b
-- move top n-1 discs from c to b using a as spare
-- e.g. 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c 
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

-- Hanoi with 4 pegs, no known optimal solution, aim for better than normal
-- hanoi which takes 2^n - 1 moves. My idea is to split stack in two move
-- top half to d using a c d then bottom to b using a b c then put top 
-- back in place
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d
  | n `mod` 2 == 0 = hanoi (n `div` 2) a d c ++ hanoi (n `div` 2) a b c ++ hanoi (n `div` 2) c b a
  | otherwise = hanoi (n `div` 2) a d c ++ hanoi (n `div` 2  + 1) a b c ++ hanoi (n `div` 2) c b a

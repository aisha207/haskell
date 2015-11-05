import Data.List (genericReplicate)

--skips "ABCD" = ["ABCD", "BD", "C", "D"]
skips :: [a] -> [[a]]
skips as = [skipo n as | n <- [1..length as]]

skipo :: Int -> [a] -> [a]
skipo n [] = []
skipo n xs
 | length xs >= n = head ys : skipo n (tail ys)
 | otherwise = []
   where ys = drop (n - 1) xs

--Find and return all the local maxima in the list
localMaxima :: [Int] -> [Int]
localMaxima [] = []
localMaxima (x:[]) = []
localMaxima (x:y:[]) = []
localMaxima (x:y:zs)
 | x > y && y < head zs = y : localMaxima (y:zs)
 | otherwise = localMaxima (y:zs)


-- Display a (horizontal) histogram of the values [0, 9]
histogram :: [Integer] -> String
histogram xs = concat [line (count (toInteger x) xs) | x <- [0..9]]

count :: Integer -> [Integer] -> Integer
count x = toInteger . length . filter (==x)

-- Draw a histogram line
line :: Integer -> String
line n = (genericReplicate n ' ') ++ "*\n"

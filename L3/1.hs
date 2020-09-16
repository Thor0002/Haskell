num2lst :: Integer -> [Int]
num2lst n = help (abs n) []

help :: Integer -> [Int] -> [Int]
help n l 
   | n < 10 = fromIntegral n:l
   | otherwise = help (div n 10) (fromIntegral (mod n 10)  : l)

lst2num :: [Int] -> Integer
lst2num = help1 0

help1 :: Integer -> [Int] -> Integer
help1 n [] = n
help1 n (h:t) = help1 (n*10 + fromIntegral h) t

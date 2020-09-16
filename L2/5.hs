powOf2 :: Integer -> Integer
powOf2 n = help n 1 0

help :: Integer -> Integer -> Integer -> Integer
help n i k
 | n < i  = -1
 | n == i = k
 | otherwise = help n (i*2) (k + 1)
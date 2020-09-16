recSum :: Integer -> [Integer]
recSum n = scanl (\tk k -> tk + (div k 3) ) 1 [1..n]
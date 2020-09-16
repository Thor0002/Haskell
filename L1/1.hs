gcd1 :: Int ->Int -> Int
gcd1 a b = 
    if b == 0 
            then a
            else gcd1 b (mod a b)

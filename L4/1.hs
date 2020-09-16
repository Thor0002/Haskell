gtMill :: Integer -> Integer
gtMill n = head $ dropWhile help [1..]
  where
    help :: Integer -> Bool 
    help k = abs (x / sin (x^2) ) <= 1000000 ||  k < n
      where 
        x = fromIntegral k

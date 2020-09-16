isPrime :: Int -> Bool
isPrime a = (a > 1) && help a 2 (sqrt (fromIntegral a) )
  
help :: Int -> Int -> Double -> Bool
help a i r =
  if fromIntegral i > r 
     then True
     else 0 /= (mod a i)  && help a (i + 1) r   
  
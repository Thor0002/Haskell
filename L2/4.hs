minmax :: Int -> Int -> Int -> Int
minmax a b c  
  | m > 10 = max (max a b) c -- max a $ max b c
  | otherwise =  m
   where 
     m = min (min a b) c
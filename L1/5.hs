maxRoot :: Int -> Int -> Int -> Double
maxRoot a b c =  
   if d < 0
      then 0/0
      else if a > 0 
              then (sqrt (fromIntegral d)  - fromIntegral b ) / fromIntegral (2*a)  
              else  (-1) * (sqrt (fromIntegral d)  + fromIntegral b ) / fromIntegral (2*a)       
   where
   d = b^2 - 4*a*c
   

             
root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps = head (head (dropWhile (\[c,d] -> d-c > eps) (iterate (help f) [a,b]) ) ) 

help :: (Double -> Double) -> [Double] -> [Double]
help f [a,b] =
   if (f c) * (f a) < 0
      then  [a,c]
      else  [c,b] 
   where
     c = (a+b)/2     

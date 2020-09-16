angle :: Double -> Double -> Double
angle x y = let t = atan2 y x 
            in if (t < 0) 
                   then 2*pi + t
                   else t
                   
angle' x y
  | a < 0     = a + 2*pi
  | otherwise = a
  where a = atan2 y x  
                   
help :: [Double] -> [Double] -> [Double]
help [xr,yr] [x,y] 
   | a1 < a2 = [x,y]
   | a1 == a2 && (x*x+y*y > xr*xr+yr*yr) = [x,y] --else  [xr,yr]
   | otherwise = [xr,yr]
    where 
      a1 = angle x y
      a2 = angle xr yr

minAngle :: [[Double]] -> [Double]
minAngle ([x,y]:t) = foldl help [x,y] t
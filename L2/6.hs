-- а)
f1 :: Double -> Double -> Double
f1 = (**) 

-- б)
f2 :: Double -> Double -> Double
f2 = (**) . (+ 1)  
 
-- subtract = flip (-) 
 
--в)
f3 :: Double -> Double -> Double
-- f3 = flip ((**) .  ((-) 3) )
-- f3 = flip ( (**) . (subtract 3) )
f3 = flip $ (**) . (subtract 3) 

-- г)
f4 :: Double -> Double -> Double
f4 =  flip (flip (**)  . (subtract 3) ) . (+ 1) 
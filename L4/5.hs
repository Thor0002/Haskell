sumSerie :: (Int -> Double) -> Double -> Double
sumSerie f eps = fst (head (dropWhile (\(sk,k) -> f (k+1) > eps) (iterate (\x -> help f x) (f 0,0)) ) ) 

help :: (Int -> Double) -> (Double,Int) -> (Double,Int)
help f (s,k) = 
   (s + (if (mod k 2) == 0 then 1 else (-1))  * (f  k), k+1)

g :: Int -> Double
g k = 1 / (fromIntegral (k+1) )

sumSerie' a eps = sum $ zipWith (*) (takeWhile (> eps) $ map a [0..]) (iterate negate 1)
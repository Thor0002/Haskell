import Data.List

cons :: a -> a -> [a]
cons a b = [a,b]

less1 :: (Fractional a, Ord a) => [a] -> [a] -> Ordering
less1 [a,b] [c,d]
  | a < c = LT
  | a == c = EQ
  |otherwise = GT


less2 :: (Fractional a, Ord a) => [a] -> [a] -> Ordering
less2 [a,b] [c,d]
  | b < d = LT
  | b == d = EQ
  |otherwise = GT
  
less1' :: (Ord t) => [t] -> [t] -> Ordering  
less1' [a,_] [c,_] = compare a c  
less2' [_,b] [_,d] = compare b d  

upperHalf :: (Fractional a, Ord a) => [a] -> [a]
upperHalf l = lp_until_med
  where 
   size = length l
   n = (size + 1) `div` 2 - 1 --if even size then (div size 2)-1 else (div size 2)
   lp = zipWith cons l (map fromIntegral [1..size] )
   slp = sortBy less1 lp
   slp_until_med = take n slp
   lp_until_med =  map head (sortBy less2 slp_until_med)
                            -- sortOn (!! 1) slp_until_med
   
upperHalf' lst =
  let 
    srtLst = sort lst
    med = srtLst !! (length lst `div` 2)
  in
    filter (>= med) lst
   
   
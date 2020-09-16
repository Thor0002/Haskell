newton :: Integer -> [Integer]
newton n = foldl (\(h:t) k -> ((help n k h):h:t) ) [1] [1..n]

help :: Integer -> Integer -> Integer -> Integer
help n k h = 
  if (n == k)
    then 1
    else div (h*(n-k+1)) k

newton' n = 
   take (n+1) $
   (!! n) $ 
   iterate (\l -> 1 : zipWith (+) l (tail l)) $
   1 : repeat 0

(!!!) (h:_) 0 = h
(!!!) (_:t) n = (!!!) t (n-1)     
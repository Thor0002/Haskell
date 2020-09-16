import Data.List

help :: [Int] -> Int -> [Int]
help [p,k,bn,bk] x
  | p == x = [p,(k+1),bn,bk]
  | otherwise = if (bk > k)
                  then [x,1,bn,bk]
                  else [x,1,p,k]
                  
mostFrequent' :: [Int] -> Int
mostFrequent' a = if (k > bk) then p else bn
   where 
    (h:t) = sort a
    [p,k,bn,bk] = foldl help [h,1,h,1] t 
    
mostFrequent :: [Int] -> Int
mostFrequent l = snd (maximum l1)
   where 
    l1 = map (\(h:t) -> (length (h:t),h) ) (group $ sort l)
    
mostFrequent' lst = head . head . sortOn (negate . length) . group . sort $ lst
import Data.Tuple

f1 :: (String,Int) -> Int -> (String,Int)
f1 p d = (+d) <$> p

f2 :: (Int,String) -> Int -> (Int,String)
f2 p d = swap (f1 (swap p) d)
 -- swap $ (+d) <$> swap p
f :: [(String,Int)] -> Int -> [(String,Int)]
f l d = (f1 d) <$> l

f1 :: Int -> (String,Int) -> (String,Int)
f1 d p = (+d) <$> p
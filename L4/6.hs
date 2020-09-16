numDigits :: Integer -> Int
numDigits n = fromIntegral ((floor ((logBase 10 (fromIntegral n)) ) + 1))

num2lst :: Integer -> [Int]
num2lst n = fst ( foldl (\(t,h) i -> ( ( (fromIntegral (mod h 10) ) : t), (div h 10) ) ) ([],n) [1..(numDigits n)] )

-- map (\x -> mod x 10) (takeWhile (> 0) (iterate (\x -> div x 10) n))

num2lst' n = snd $ head $ dropWhile ((/= 0) . fst) $ iterate (\(k,lst) -> (k `div` 10, k `mod` 10 : lst)) (n,[])

allDigits :: Integer -> [Int]
allDigits n = concat (map num2lst [1..n] ) 
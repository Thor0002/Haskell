sumSquares1 :: (Num a) => [a] -> a
sumSquares1 [] = 0
sumSquares1 (h:t) = h*h + sumSquares1 t

sumSquares2 :: (Num a) => [a] -> a
sumSquares2 = help 0
help :: (Num a) => a -> [a] -> a
help s [] = s
help s (h:t) = help (s + h*h) t

sumSquares3 :: (Num a) => [a] -> a
sumSquares3 = foldl (\s x -> s + x*x) 0 
sumOfDigits :: Integer -> Int
sumOfDigits 0 = 0
sumOfDigits n = fromIntegral (abs (rem n 10) ) + sumOfDigits (quot n 10)
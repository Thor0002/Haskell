reverseNumber :: Integer -> Integer
reverseNumber n = help n 0

help :: Integer -> Integer -> Integer
help n r = 
     if n == 0
        then r
        else help (div n 10) (r*10 + (mod n 10) )
             
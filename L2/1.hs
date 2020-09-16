dFact :: Integer -> Integer
dFact 0 = 1
dFact 1 = 1
dFact n = n * dFact (n-2)

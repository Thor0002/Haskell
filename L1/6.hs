root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps = 
     if b-a <= eps
        then a
        else if (f c) * (f a) < 0
             then root f a c eps
             else root f c b eps 
     where
     c = (a+b)/2     

f :: Double -> Double
f x = x^3   
import Control.Monad.Reader

find_root :: (Double -> Double) -> Double -> Double ->  Double -> Double
find_root f e a b 
  | t = a' 
  | otherwise = find_root f e a' b'
  where
   mreader = runReader (help f e)
   (t,(a',b')) = mreader (a,b)
  

help :: (Double -> Double) -> Double -> Reader (Double,Double) (Bool, (Double, Double))
help f e = do
   (a,b) <- ask
   let c = (a+b)/2 
       t = (abs (b-a)) < e
   return (if (f c) * (f a) < 0 then (t,(a,c)) else  (t,(c,b)) )
     
     
find_root' f eps a b = runReader (help' a b) (f,eps)     
     
--          a         b                     f         eps   корень     
help' :: Double -> Double -> Reader (Double->Double,Double) Double
help' a b = do
-- (\(Reader ask _) a b -> ...) ???
  (f,e) <- ask
  let c = (a+b)/2
  if b-a < e 
    then return c
    else if f c * f a < 0 then help' a c else help' c b
     
root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps = 
   let help a b = 
     let c = (a+b)/2 in 
         if b-a <= eps
            then a
            else if (f c) * (f a) < 0
                 then help a c 
                 else help c b 
   in
     help a b   

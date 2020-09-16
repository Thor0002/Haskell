{-# LANGUAGE FlexibleInstances #-}

class Group a where
  (.+.) :: a -> a -> a
  (.-.) :: a -> a -> a
  zero :: a
  opposite :: a -> a
  
  x .-. y = x .+. opposite y
  opposite x = zero .-. x
  {-# MINIMAL (.+.), zero, (opposite | (.-.)) #-}

instance Group Double where
  (.+.) = (+)
  zero = 0 
  opposite = negate 
  
-- ____________________________________________________________________  

class (Group a) => (LinearSpace a) where
  scalar :: Double -> a -> a

data Vector a = Vector
  { dim :: Int, coord :: [a] } deriving(Show,Eq)
 
instance Group a => Group [a] where
  (.+.) = zipWith (.+.)
  zero = repeat zero
  (.-.) = zipWith (.-.)
  
instance LinearSpace a => LinearSpace [a] where
  scalar alpha = map (scalar alpha)
  
instance Group a => Group (Vector a) where
  v1 .+. v2 
   | dim v1 == 0 = v2
   | dim v2 == 0 = v1
   | dim v1 == dim v2 = Vector (dim v1) $ zipWith (.+.) (coord v1) (coord v2) 
   | otherwise = error "Addition of vectors of different dimensions!"
  zero = Vector 0 []
  opposite v = Vector (dim v) $ map opposite $ coord v 

instance LinearSpace a => LinearSpace (Vector a) where
  scalar alpha v = Vector (dim v) $ map (scalar alpha) $ coord v   
 
instance Group a => Group (b -> a) where
  f .+. g = \x -> f x .+. g x
  zero = \_ -> zero
  opposite f = \x -> opposite $ f x
  
instance (LinearSpace a) => (LinearSpace (b -> a) ) where
  scalar alpha f = scalar alpha . f
  
-- ____________________________________________________________________
  
class (LinearSpace a) => (HilbertSpace a) where
  (%) :: a -> a -> Double
  
help :: (Double -> Double) -> (Double -> Double) -> Double
help f g = foldl1 (\res x -> (res + (f x) * (g x) * (1 / 1000) ) ) ( 0 : map (* (1 / 1000) ) [1..1000] )

instance LinearSpace Double where
  scalar alpha = (alpha *)

n :: Int
n = 1000

instance HilbertSpace (Double -> Double) where
 f % g = let xs = map (/ fromIntegral n) [0..fromIntegral n] in sum $ zipWith (*) (map f xs) (map g xs)

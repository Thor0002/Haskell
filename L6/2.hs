class Monoid' a where
  (.+.) :: a -> a -> a
  zero :: a

class (Monoid' a) => (Group a) where
  (.-.) :: a -> a -> a
  opposite :: a -> a

  x .-. y = x .+. opposite y
  opposite x = zero .-. x
  {-# MINIMAL (opposite | (.-.)) #-}

instance Monoid' [a] where
 x .+. y = x ++ y
 zero = []
--___________________________________________________________

data Transposition = Transposition Int [Int] deriving(Show)

instance Monoid' Transposition where
 zero = (Transposition 0 [1..])
 t1@(Transposition n1 p1) .+. t2@(Transposition n2 p2) 
   | n1 == 0 = t2
   | n2 == 0 = t1
   | n1 /= n2 = error "!!!"
   | otherwise = Transposition n1 $ map (p1 !!) $ map (subtract 1) p2
  
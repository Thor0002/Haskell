import Control.Monad.State

--f :: Int -> Double
f n = let a = g in g
  where
    g :: State Double ()
    g = do
       k <- get
       let x = 11*k + pi
       put (x - fromIntegral (floor x) )
       return ()
    generator = execState g 0.123
    help :: Int -> Double -> Int
    help k s
      | s > 1 = k
      | otherwise = help (k+1) (s + generator)
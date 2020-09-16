import System.Random
import Control.Monad
import Data.List

f1 :: Int -> IO [(Int,Double)]
f1 n = map (\(s,k) -> (s,fromIntegral k/fromIntegral n)) <$> f n

f :: Int -> IO [(Int,Int)]
f n = do
   l <- replicateM n $ replicateM 3 $ randomRIO (1,6::Int)
   let 
    l1 = map (\x -> (head x, length x) ) $ group $ sort $ map sum l
   return l1
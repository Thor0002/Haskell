import Control.Monad.Writer

data Tree a = Empty | Node a (Tree a) (Tree a) deriving(Show)

f :: Int -> Tree Int -> Writer String Bool
f _ Empty = do
  tell "Not found"
  return False
f el (Node n l r)
  | el == n   = do
       tell $ show n 
       return True
  | el < n     = do
       tell $ show n ++ " -> "
       f el l
  | otherwise =  do
       tell $ show n ++ " -> " 
       f el r
   
   

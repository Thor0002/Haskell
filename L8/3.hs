import Control.Monad.Writer

data Tree a = Empty | Node a (Tree a) (Tree a) deriving(Show)

help :: Writer String Bool
help = do
       tell ""
       return False

eq :: Int -> Int -> Bool -> Writer String Bool
eq n x _ = do
        tell ((show n)  ++ "->")
        return (x == n) 

f :: Int -> Tree Int -> (Bool, String)
f x t@(Node n t1 t2) = help1 x t help
   
help1 _ Empty r = runWriter r
help1 x (Node n t1 t2) r 
  | x == n    = runWriter r1
  | x > n     = help1 x t2 r1
  | otherwise = help1 x t1 r1
  where 
   r1 = r >>= (eq n x)
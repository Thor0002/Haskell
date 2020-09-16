import Control.Monad.Writer

help :: Int ->  Writer String Int
help n = do
       tell (show n)
       return n

max1 :: Int -> Int -> Writer String Int
max1 a b = do
        let newMax = max a b
        tell $ " -> " ++ show newMax 
        return newMax

f (h:t) = runWriter $ foldl (\r m -> r >>= (max1 m) ) (help h) t 

--------------------------------

f' (h:t) = runWriter $ do
  tell $ show h
  foldM max1 h t 
     


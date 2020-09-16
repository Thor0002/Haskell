import Control.Monad.State
f :: [Int] -> [Int]
f l = execState (help l) [0,0,0,0,0,0,0,0,0]

get_first_digit :: Int -> Int
get_first_digit n = fromEnum (head (show n) ) - 48

updateList :: Int -> [Int] -> [Int]
updateList i (h:t)
  | i == 1 = h + 1 : t
  | otherwise = h : (updateList (i-1) t)

help :: [Int] -> State [Int] [Int]
help [] = return []
help (h:t) = do
--  l <- get
--  put (updateList (get_first_digit h) l)
  modify $ updateList $ get_first_digit h
  help t
  
  
{-  
foo :: State Double Double
foo = do  
  xi <- get
  let xiNew = ...
  put xiNew
  return xiNew
  
baz :: ... -> State Double ???
baz ... = do
  rand <- foo
  rand1 <- foo
-}
  
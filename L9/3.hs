import Data.Char
import System.IO
import Control.Monad
import Data.List

main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    help 1 1000000

help :: Int -> Int -> IO ()
help a b = do
  let x = (a + b) `div` 2 
  putStrLn $ show x
  hSetBuffering stdin NoBuffering
  --hSetEcho stdin False
  c <- getChar
  void getChar 
  case c of
    '>' -> help (x+1) b
    '<' -> help a x
    _ -> return ()
        


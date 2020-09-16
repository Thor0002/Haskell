import Data.Char
import System.IO
import Data.List
import System.Random

main :: IO ()
main = do

  num <- randomRIO (1,1000000)
  let 
    order :: Int -> Ordering
    order n = compare num n  
    
    help  :: IO ()  
    help = do
      s <- getLine
      let
        n = (read s) :: Int
        t = order n
      case t of
        LT -> do 
              putStrLn "<"
              help
        GT -> do
              putStrLn ">"
              help
        EQ -> do
              putStrLn "="
              return ()
  help          
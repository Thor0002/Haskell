import Data.Char
import System.IO
import Data.List

main = do
  input <-  openFile "input.txt" ReadMode
  output <- openFile "output.txt" WriteMode
  str <- hGetContents input
  hPutStr output (f str)
  hClose input
  hClose output

-- [a, b] <- (map read . words) `fmap` getLine

f :: String -> String
f s = let x = map read (words s) :: [Int] in unwords $ map show (sortBy (flip compare) x)

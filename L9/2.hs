import Data.Char
import System.IO
import Data.List

amount :: String -> Char -> Int
amount [] c = 0
amount (h:t) c 
  | h == c = 1 + amount t c
  | otherwise = amount t c

main = do
  input <-  openFile "input.txt" ReadMode
  output <- openFile "output.txt" WriteMode
  str <- hGetContents input
  hPutStr output (f str)
  hClose input
  hClose output

f :: String -> String
f s = unlines (map (get s) [' '..'~'])

get :: String -> Char -> String
get s c = "('" ++ [c] ++ "':" ++ (show $ amount s c) ++ ")"
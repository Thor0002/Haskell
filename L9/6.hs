import Data.List
import System.IO
import Data.Char

main = do
  input <-  openFile "input.txt" ReadMode
  output <- openFile "output.txt" WriteMode
  str <- hGetContents input
  hPutStr output (unlines $ f str)
  --print (f str)
  hClose input
  hClose output

f :: String -> [String]
f s = reverseDictionaryByLines 
  where
   listOfLines = (map (delete "-") . map words . lines) s
   reverseDictionaryByLinesInPairs = map (\(h:t) -> map (\x -> (x,h)) t) listOfLines
   sortAllReverseDictionaryTogether = sort $ concat reverseDictionaryByLinesInPairs
   groupedReverseDictionary = groupBy (\x y -> fst x == fst y) sortAllReverseDictionaryTogether
   reverseDictionaryByLines = map (\x -> (fst $ head x) ++ " - " ++ (unwords $ map snd x)) groupedReverseDictionary 

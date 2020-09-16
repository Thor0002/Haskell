posMax :: [String] -> String
posMax [] = ""
posMax lst = foldl1 (zipWith max) lst
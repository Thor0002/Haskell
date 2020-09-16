import Data.Char
import Data.List

freqs :: [String] -> [(Char,Int)]
freqs l = sort_list_pair
  where
    l_with_abc = filter (\c -> ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z') ) (concat l)
    l_with_ABC = map toUpper l_with_abc
    sort_l = sort l_with_ABC
    group_l = group sort_l
    list_pair = map (\(h:t) -> (h, (length (h:t) ) ) ) group_l
    sort_list_pair = sortBy (\(a,b) (c,d) -> compare d b) list_pair
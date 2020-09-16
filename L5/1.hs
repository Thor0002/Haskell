myZip1 :: ([a],[b]) -> [(a,b)]
myZip1 ([],_) = []
myZip1 (_,[]) = []
myZip1 ((h1:t1),(h2:t2)) = ((h1,h2) : (myZip1 (t1,t2) ) ) 

help :: ([a],[b]) -> Bool
help (a,b)  = (not (null a) ) && (not (null b) )

help1 :: ([a],[b]) -> (a,b)
help1 (a,b) = (head a, head b)
        
myZip2 :: ([a],[b]) -> [(a,b)]
myZip2 (a,b) = map help1 (takeWhile help (iterate (\(l1,l2) -> (tail l1, tail l2) ) (a,b) ) )

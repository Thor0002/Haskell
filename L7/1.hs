f :: [a] -> [b] -> [(a,b)]
f l1 l2 = ([(,)] <*> l1) <*> l2

-- (,) <$> l1 <*> l2
f :: [Maybe Int] -> [Maybe Int]
f l = ( [(<*>) . (<*>) (Just (+))] <*> l ) <*> [Just 3]
  -- ((+3) <$>) <$> lst
  -- ((<$>) . (<$>)) (+3) lst
  -- (fmap . fmap) (+3) lst
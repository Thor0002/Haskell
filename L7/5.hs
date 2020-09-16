import Data.List
data Point = Point { x :: Double, y :: Double } deriving(Show,Ord,Eq)
data Line = Line { a :: Double, b :: Double, c :: Double } deriving(Show)

lines_intersection :: Line -> Line -> Maybe Point
lines_intersection l1 l2 =
  if d == 0 then Nothing else Just t
  where
    d = a l1 * b l2 - a l2 * b l1
    t = Point ((b l1*c l2 - b l2*c l1)/d) ((c l1*a l2 - c l2*a l1)/d)

max_line :: [Line] -> Maybe Point
max_line l = maximum $ uncurry lines_intersection <$> f l l    

f :: [a] -> [b] -> [(a,b)]
f l1 l2 = (,) <$> l1 <*> l2    
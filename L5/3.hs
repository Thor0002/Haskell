import Data.List
data Suit = Spades | Clubs | Diamonds | Hearts deriving (Eq, Show)
data Value = Minor Int | Jack | Queen | King | Ace deriving (Eq, Ord, Show)
data Card = Card Suit Value deriving (Eq, Show) 

isMinor :: Card -> Bool
isMinor (Card _ (Minor _) ) = True
isMinor (Card _ _) = False

cardsMinMax :: [Card] -> (Card,Card)
cardsMinMax l = (minimumBy (\(Card _ v1) (Card _ v2) -> compare v1 v2) l, 
                 maximumBy (\(Card _ v1) (Card _ v2) -> compare v1 v2) l)

sameSuit :: Card -> Card -> Bool
sameSuit (Card s1 _) (Card s2 _) = s1 == s2

beats :: Card -> Card -> Bool
beats (Card s1 v1) (Card s2 v2) = s1 == s2 && v2 < v1

beatsTrump :: Card -> Card -> Suit -> Bool
beatsTrump (Card s1 v1) (Card s2 v2) s 
 | s == s1 && s == s2 = v1 > v2
 | s == s1 = True
 | s1 == s2 && v1 > v2 = True
 | otherwise = False

beatsList :: [Card] -> Card -> Suit -> [Card]
beatsList l c s = filter (\c1 -> beatsTrump c1 c s) l
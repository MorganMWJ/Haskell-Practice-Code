module Practice where

data Day = Mon | Tues | Weds | Thur | Fri | Sat | Sun deriving (Eq,Show)

isPayDay :: Day -> Bool
isPayDay day
	| day==Fri = True
	| otherwise = False
	
intToDay :: Int -> Maybe Day
intToDay number = 
	case number of
		1 -> Just Mon
		2 -> Just Tues
		3 -> Just Weds 
		4 -> Just Thur 
		5 -> Just Fri
		6 -> Just Sat 
		7 -> Just Sun
		_ -> Nothing
		
data Triangle = NotATriangle | Equilateral | Isoceles | Scalene deriving (Eq)
instance Show Triangle where
	show NotATriangle = "Not a Valid Triangle"
	show Equilateral  = "Equilateral"
	show Isoceles     = "Isoceles"
	show Scalene      = "Scalene"

sidesToTriangle :: (Int,Int,Int) -> Triangle
sidesToTriangle (0,s2,s3) = NotATriangle
sidesToTriangle (s1,0,s3) = NotATriangle
sidesToTriangle (s1,s2,0) = NotATriangle
sidesToTriangle (s1,s2,s3)
	| (s1==s2) && (s2==s3) = Equilateral
	| s1/=s2 && s1/=s3 && s2/=s3 = Scalene
	| otherwise = Isoceles
-------------------------------------------------------------------------------------	
data Suit = Clubs | Diamonds | Hearts | Spades
                      deriving (Eq, Ord, Show)

data Value = Number Int | Jack | Queen | King
                      deriving (Eq, Ord)

data PlayingCard = Card Value Suit deriving (Eq, Ord)
	
instance Show Value where
	show (Number 1) = "Ace"
	show (Number n) = show n
	show Jack       = "Jack"
	show Queen      = "Queen"
	show King       = "King"
	
instance Show PlayingCard where
	show (Card val suit) = 
		"The " ++ (show val) ++ " of " ++ (show suit)

allowedValues :: [Value]
allowedValues =
	[ Number n | n <- [1..10]] ++ [Jack,Queen,King]
	
allowedSuits :: [Suit]
allowedSuits = [Clubs,Diamonds,Hearts,Spades]

makePack :: Pack
makePack = [ Card v s | v <- allowedValues, s <- allowedSuits ]
-- A Pack and a Hand are just lists of PlayingCards

type Pack = [PlayingCard]
type Hand = [PlayingCard]

snap :: (PlayingCard, PlayingCard) -> Bool
snap (Card v1 s1,Card v2 s2) = if v1==v2	
								then True
								else False


snapGame :: Hand -> Hand -> Bool    
snapGame cs1 cs2 = any snap (zip cs1 cs2) 
--return true if any of the pair's of cards played will snap

--for practice
hand1 :: [PlayingCard]
hand1 = [Card (Number 7) Diamonds, Card King Clubs, Card (Number 1) Hearts]

hand2 :: [PlayingCard]
hand2 = [Card (Number 3) Diamonds, Card King Spades, Card Jack Spades]


foobarPack :: Pack	
foobarPack = subPack ++ subPack
	where 
		subPack = [ Card value suit | value <- someValues, suit <- [Clubs,Diamonds,Spades,Hearts] ]
			where 
				someValues = [Number x | x <- [6..10]] ++ [Jack,Queen,King]
				
foobarPack' :: Pack
foobarPack' = 
	filter higher6 (makePack ++ makePack)
		where
			higher6 (Card v s) = v>Number 6
			

				
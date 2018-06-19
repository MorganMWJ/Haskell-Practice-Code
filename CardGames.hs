module CardGames 

where
--if you are using specific instances of show you cannot derive the default one from the data type
data Suit = Clubs | Diamonds | Hearts | Spades
                      deriving (Eq, Ord, Show)

data Value = Number Int | Jack | Queen | King
                      deriving (Eq, Ord)

data PlayingCard = Card Value Suit 


-- Some example cards, try defining others.

myCard    = Card (Number 9) Diamonds
yourCard  = Card Queen Hearts
firstCard = Card (Number 2) Clubs
aceOfDiamonds = Card (Number 1) Diamonds


-- Define your own Show instances here for elements of 
-- the Value and PlayingCard types (remember to remove 
-- the automatically derived Show from the data definitions)
instance Show Value where
	show (Number 1) = "Ace"
	show (Number n) = show n
	show Jack       = "Jack"
	show Queen      = "Queen"
	show King       = "King"
	
instance Show PlayingCard where
	show (Card val suit) = 
		"The " ++ (show val) ++ " of " ++ (show suit)
---------------------------------------------------------
-- beats tells us if one card has a higher value than another

beats :: PlayingCard -> PlayingCard -> String
beats (Card val1 suit1) (Card val2 suit2) 
	| val1>val2 = "First card is greater than the second."
	| val1<val2 = "First card is lesser than the second."
	| otherwise = "Neither card wins, both are equal."

---------------------------------------------------------
allowedValues :: [Value]
allowedValues =
	[ Number n | n <- [1..10]] ++ [Jack,Queen,King]
	
allowedSuits :: [Suit]
allowedSuits = [Clubs,Diamonds,Hearts,Spades]

-- A Pack and a Hand are just lists of PlayingCards

type Pack = [PlayingCard]
type Hand = [PlayingCard]

-- makePack will create the full pack of cards 
-- using a list comprehension
makePack :: Pack
makePack = [ Card v s | v <- allowedValues, s <- allowedSuits ]
---------------------------------------------------------
-- deal will give you the top n cards from a pack, 
-- and the remainder of the pack

dealFromDeck :: Int -> (Hand,Pack)
dealFromDeck n = (take n pack, drop n pack) 
	where
		pack = makePack

snapGame :: Hand -> Hand -> Bool
snapGame cs1 cs2 = zipWith (snap) cs1 cs2 
	where 
		snap :: (PlayingCard, PlayingCard) -> Bool 
		snap p1 p2 =
			if p1==p2 
				then True
				else False
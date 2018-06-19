module Revision where
--Algebraic data types

--basic data types (Bool, Char, Float..etc)
--example of defining data types and values they can take
-- data Bool = True | False
--All data types and synonyms must start with a capital letter

--data type for days of week
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun 
	deriving Eq
--function to use this data type
isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False
--for this to work we need to derive Eq so that we can use the '==' operator on types of day
isWeekend' :: Day -> Bool 
isWeekend' day 
	| day == Sat = True 
	| day == Sun = True
	| otherwise = False

isWorkDay = not . isWeekend --returns opposite of isWeekend, using the 'not' function and dot notation

isPayDay :: Day -> Bool
isPayDay Fri = True
isPayDay _   = False
-----------------------------------------------------------------

type LCardNum   = (Int, Int, Int, Int)
type AmCardNum = (Int, Int, Int)

type Date   = (Int, Int)

type Since = Int

data PaymentCard =   Visa LCardNum Date --LCardNum and Date are just info that this data value carries
                   | Mastercard LCardNum Date
                   | Amex AmCardNum Date Since

--here are some predefined cards/some variables of the 'PaymentCard' data type
myCard = Amex (4444, 333333, 11111) (8, 15) 79

deptCard = Visa (5555, 6666, 2222, 1111) (12, 14)


isAcceptedCard :: Date -> PaymentCard -> Bool
isAcceptedCard currentDate (Visa num exp)       = isWithinExpiry exp currentDate
isAcceptedCard currentDate (Mastercard num exp) = isWithinExpiry exp currentDate
isAcceptedCard currentDate _                    = False

-- isWithinExpiry should take an expiry date and the current date and check if the card has not yet expired

isWithinExpiry :: Date -> Date -> Bool
isWithinExpiry (exp_month, exp_year) (curr_month, curr_year) 
	| exp_year<curr_year = False 
	| exp_year>curr_year = True
	| otherwise          = if exp_month>curr_month
								then True
								else False

data Shape = 	Square Side
				| Rectangle Width Length
				| Circle Radius

type Side = Double
type Width = Double
type Length = Double
type Radius = Double 

--list of some shapes
myShape = Rectangle 4.3 5.8
coolShape = Circle 6.2
basicShape = Square 1 

isCircle :: Shape -> Bool
isCircle (Circle r) = True
isCircle _          = False 
--can use '_' instead of 'r' because 'r' isnt used on right side of equation
isCircle' :: Shape -> Bool
isCircle' (Circle _) = True
isCircle' _          = False 


isRed :: Suit  -> Bool
idRed Hearts   = True
isRed Diamonds = True 
isRed _        = False

--(Eq allows us to use '==' and '/=' operators)
isRed' suit = 
	suit == Diamonds || suit == Hearts 
----------------------------------------------------------------------------------------------	
--type-class is a collection of types that share common operands.
--all basic types are members of the Eq type class.
--Example type class': Ord, Eq, Num, Show
--if we are using (our own) algebraic data types we have to derive a type-class 
--to use its operands.

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Show)
data Value = The Int | Jack | Queen | King | Ace deriving (Eq, Ord, Show)
data PlayingCard = Card Value Suit deriving (Eq, Ord, Show)
--when Ord is derived the greatest value is on the right side 
--so for the 'value' data type 'The' is Lesser than 'Queen'
--and 'Jack' is Lesser than 'King'
--so Jack>King would return False


--cards 
card1 = Card (The 7) Diamonds
card2 = Card (Ace) Hearts
bullet = Card Ace Spades

printCard :: PlayingCard -> String
printCard (Card val suit) = 
	if val < Jack 
		then (show val) ++ " of " ++ (show suit) 
		else "The " ++ (show val) ++ " of " ++ (show suit)
		
--Summary		
--We used the keyword data to make new types.
--Types constructed in this way are known as Algebraic Data Types
--The new type has a name
--The new type has data constructors which are named
--The data constructors can take arguments
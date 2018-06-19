module Revision where


isSmall :: Int -> Bool
isSmall x = x<5--no need for if/else for one boolean operator

greet :: String -> String 
greet x = "Good Day " ++ x ++ "!"

fiveTimes :: String -> String
fiveTimes s = 
	s ++ " " ++ s ++ " " ++ s ++ " " ++ s ++ " " ++ s

isLetterZ :: Char -> Bool
isLetterZ c =
	c == 'Z'

isLetterZ' :: Char -> Bool
isLetterZ' letter 
	| letter == 'Z' = True
	| letter == 'z' = True
	| otherwise = False
	
rectangleArea :: Int -> Int -> String
rectangleArea x y =
	"Area of Rectangle = " ++ show (x*y)

biggerThan :: Int -> Int -> Bool	
biggerThan x y = x>y

biggerThan' :: Int -> Int -> String
biggerThan' x y = 
	if x>y 
		then show (x) ++ " is bigger than " ++ show (y)
		else show (x) ++ " is not bigger than " ++ show (y)
	
joinStrings :: String -> String -> String -> String
joinStrings x y z = 
	"Here are your three strings concatenated: " ++ x ++ y ++ z
	
chooseCoat :: Int -> Int -> String
chooseCoat temp rainfall =
	if (temp<10) && (rainfall > 5)
		then "Stay The Fuck In!"
		else if temp<10
			then "Wear a duffle coat"
			else if rainfall > 5
				then "Rain Mac it is!"
				else "summer jacket:-)"		
				
				

	
	



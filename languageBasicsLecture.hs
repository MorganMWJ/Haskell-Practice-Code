module Revision where

birthdayMessage :: Int -> String
birthdayMessage age =
	if age>100 
		then "Happy Birthday from the Queen!"
		else if age<20
			then "Happy Birthday, you're " ++ (show (abs age)) --abs, is like modulus |-3| => 3
			else "Happy Birthday, another year older."
			
birthdayMessage' :: Int -> String
birthdayMessage' age
	|age>100 = "Happy Birthday from the Queen!"
	|age<20  = "Happy Birthday, you're " ++ (show (abs age))			
	|otherwise = "Happy Birthday, another year older."
-------------------------------------------------------------------------------------------	
--WHERE
--where allows local definitions after the main body of the function.
--these can be local variables or local functions
	
hoodMessage :: String -> String 
hoodMessage name = greetBlid ++ " , just a reminder yo... " ++ reminder 
	where 	--must indent lesser variables/functions properly
		greetBlid = "Welcome to da hood " ++ name -- local variable definition
		reminder = "talk shit. get hit." --local variable definition
		
--let and in are always used together
hoodMessage' :: String -> String 
hoodMessage' name =
	let 
		greetBlid = "Welcome to da hood " ++ name -- local variable definition
		reminder = "talk shit. get hit." --local variable definition
	in
		greetBlid ++ " , just a reminder yo... " ++ reminder
-------------------------------------------------------------------------------------------		
sumDiff :: Int -> Int -> Int
sumDiff num1 num2 = 
	let 
		s = num1+num2--finds sum
		d = abs(num1-num2)--finds positive difference
	in 
		(s*s) + (d*d)
		
sumDiff' :: Int -> Int -> Int
sumDiff' num1 num2 = (sum*sum) + (diff*diff)
	where	
		sum = num1 + num2
		diff = abs(num1-num2)
		
-------------------------------------------------------------------------------------------	
--case-of

review :: String -> String
review bandName = 
	case bandName of
		"Rise Against" -> "Revolutionary"
		"Vance Joy"    -> "Nostalgic"
		"Shakira"      -> "She can dance!"
		_              -> "Not in our review options. :("

type Count = Int
type Sound = String		
speak :: Count -> Sound
speak number = 
	case number of
		1 -> "One"
		2 -> "Two"
		3 -> "Three"
		4 -> "Four"
		_ -> "Cant yet speak that number."		
-------------------------------------------------------------------------------------------	
--function application
square   :: Int -> Int
square x = x * x
--using other functions inside functions
quad :: Int -> Int
quad x = square (square x)

chopOffEnd :: String -> String
chopOffEnd cs = reverse(tail(reverse cs))

chopOffEnd' :: [a] -> [a]
chopOffEnd' = reverse . tail . reverse 

		



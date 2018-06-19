
--higher order functions
--Example of Partial Application
--where a function is another function with one of its arguments already provided
add :: Int -> Int -> Int
add x y = x + y

addFour' x = add 4 x
addFour = add 4
--both the same function
--when comiled and run addFour 6 will return 10
--addFour 6 will be replaced with add 4 6 => 10

receiptPrint :: String -> Double -> String
receiptPrint shop cost = 
	"Receipt from " ++ shop ++ " for : " ++ (show cost)
	
sparReceipt = receiptPrint "Spar"--partial application function
--the functions are made up of another function with one argument already given
coopReceipt = receiptPrint "Co-op"--partial application function
--these functions only need a double to be given to them they already have the string 
--------------------------------------------------------------------------------------------------------
--Functions as Arguments

twice :: (a -> a) -> a -> a
twice function argument = function (function argument)

--example map takes a function as argument
map' :: (a -> b) -> [a] -> [b]
map' function [] = []
map' function (x:xs) = function x : map function xs 

--zip [1,2,3] [11,12,13]
-- =>[(1,11),(2,12),(3,13)]
--zipWith (+) [1,2,3] [11,12,13]         zipWith is a higher order function
--[12,14,16]
--zipWith (*) [1,2,3] [11,12,13]         it takes a function as a parameter
--[11,24,36]
--------------------------------------------------------------------------------------
notNull :: [a] -> Bool
notNull x 
	| null x = False
	| otherwise = True
----------------------------------------------------------------------------------------
--point-free style
chopEnds = reverse . tail . reverse . tail 

--other style
replaceEndOfList :: [a] -> a -> [a]
replaceEndOfList list x = reverse (x: (drop 1 (reverse list)))			
----------------------------------------------------------------------------------------------
--Lambda Expression
--function with no name 
countGroup :: [a] -> (a,Int)
countGroup list = (head list,length list)

-- \list -> (head list,length list)--lambda expression
--instead of using map countGroup list 
--we can:
function list = map (\xs -> (head xs, length xs)) list

square :: Int -> Int 
square x = x*x
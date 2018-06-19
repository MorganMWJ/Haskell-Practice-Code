--factorial with pattern matching
factorial :: Int -> Int
factorial 0 = 1                  --if given 0 return 1
factorial n = n * factorial (n-1)--if given any other int return this

--factorial using guards
factorial' :: Int -> Int
factorial' x
	| x==0 = 1
	| x/=0 = x * factorial (x-1)--this function has conditionals as guards 
--map written recursively	
map' :: (a -> b) -> [a] -> [b]--polymorphic type
map' function []     = []
map' function (head:restOfList) = (function head) : map' function restOfList 
--filter written recursively
filter' :: (a -> Bool) -> [a] -> [a]--polymorphic type
filter' boolFunction []     = []
filter' boolFunction (x:xs) =
	if boolFunction x
		then x : filter boolFunction xs
		else filter' boolFunction xs
--zip written recursively
zip' :: [a] -> [b] -> [(a,b)]--polymorphic type
zip' _ []          = []	-- '_' = anything use it when parameter is not needed on right side of equals sign
zip' [] _          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
-------------------------------------------------------------------------------------------------------------
--Accumulation recursion
reverse' :: [a] -> [a]
reverse' []    = []
reverse' [x]   = []
reverse' list  = accumulator [] list --function has spare list/space to move from front of one list onto end of emty list
	where 
		accumulator holder []     = holder
		accumulator holder (x:xs) = accumulator (x:holder) xs 
		
------------------------------------------------------------------------------------------
mySort :: [Int] -> [Int]
mySort [] = []
mySort (x:xs) = insert x (mySort xs)
	where 
		insert :: Int -> [Int] -> [Int]
		insert number [] = [number]
		insert number (headOfList:restOfList) = 
			if number < headOfList
				then number : headOfList : restOfList
				else headOfList: insert number restOfList 
--easier function inserting one number in correct place at a time
------------------------------------------------------------------------------------------
largest :: [Int] -> Int
largest []     = 0
largest [x]    = x
largest (x:xs) = 
	if biggestOf x xs --will return a true or false if x is biggest in list if true gives x else sees if next item is biggest
		then x
		else largest xs
	where
	biggestOf :: Int -> [Int] -> Bool
	biggestOf x []   = True
	biggestOf x (y:ys) = 
		if x<y
			then False
			else biggestOf x ys

myTake :: Int -> [a] -> [a]
myTake 0 list = []
myTake n [] = []
myTake n (head:tail)= head : myTake (n-1) tail 

myDrop :: Int -> [a] -> [a]
myDrop 0 list   = list
myDrop n []     = []
myDrop n (x:xs) = myDrop (n-1) xs 











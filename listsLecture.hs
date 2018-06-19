module Revision where 

--list items must all be of the same type
--different from tuples which can contain data of mixed types

isSpecialNumber :: Int -> Bool
isSpecialNumber x = 
	elem x specialList --elem = see's if an element(x) is inside a list(specialList)
	where
		specialList = [1,2,3,5,7,11]
-------------------------------------------------------------------------------------------		
-- appending lists "Hello" ++ " world"
-- adding onto front of a list 0:specialList

-- 3:[7]       => [3,7]
-- []++[5,6]   => [5,6]
-- []++[[5,6]] => [[5,6]]
-- 8:[]        => [8]
-- []:[[5,6]]  => [ [],[5,6] ] 
-- []:[5]      => error!!
-- [[]]:[[5,6]]=> error!!  can never cons empty list on unless it is a list of lists

addToEnd :: a -> [a] -> [a]
addToEnd x xs = xs ++ [x] 
--adding item to end of a list takes whole list
-- to be traversed by compiler therefore inefficient to use
-------------------------------------------------------------------------------------------
-- head => returns first item of a list
-- head :: [a] -> a
-- tail => returns the list without first item
-- tail :: [a] -> [a]
chopOffEnds :: [a] -> [a]
chopOffEnds []     = []
chopOffEnds [x]    = [x]
chopOffEnds list = reverse (tail (reverse (tail list)))
--this function is written with a form of pattern matching
--function has different variations dependent on type of input

--take :: Int -> [a] -> [a]                          drop :: Int -> [a] -> [a]
-- returns first n items in a list                   removes the first n items in a list
chopOffEnds' :: [a] -> [a]
chopOffEnds' []    = []
chopOffEnds' [x]   = [x]
chopOffEnds' list = drop 1 (take (n-1) list)
	where 
		n = length (list)
--takeWhile (<5) [1,2,3,4,5,6,7] => [1,2,3,4]
--takes a function producing a boolean and applies to list, keeps all items that meet true condition
--will stop when reached an item that meets false so best to use on sorted lists
--dropWhile  (<7) [1,2,3,6,8,9] => [8,9]
--takes a function producing a boolean and applies to list, removes all items that meet true condition
--will stop when reached an item that meets false so best to use on sorted lists
firstFive :: [a] -> [a]
firstFive xs = take 5 xs

halfOf :: [a] -> [a]
halfOf xs = drop half xs
	where 
		half = div (length xs) 2 --div is function for integer division (normal / is for floats)

-----------------------------------------------------------------------------------------------------
--pattern matching on lists
isComment :: String -> Bool
isComment ('-':'-':restOfString) = True
isComment _                      = False
--isComment written without pattern matching
isComment' :: String -> Bool
isComment' string = 
	if (take 2 string) == ['-','-']
		then True
		else False

isEmpty :: [a] -> Bool		
isEmpty []     = True
isEmpty (x:xs) = False

isEmpty' :: [a] -> Bool		
isEmpty' []     = True
isEmpty' list   = False

isEmpty'' :: [a] -> Bool		
isEmpty'' [] = True
isEmpty'' _  = False

sayTimes :: Int -> String
sayTimes 1 = "Once"
sayTimes 2 = "Twice"
sayTimes 3 = "Thrice"
sayTimes otherNum = (show otherNum) ++ " times"

------------------------------------------------------------------------------------------------

---common list functions

--null 
--	if list is empty => True 
--  if not => False

--length
--	gives count of items in a list

--zip
--	pair up two lists to make a list of pairs(tuples)

--concat
-- 	turn list of lists into a single list

--map
--	apply function to every item in a list 

--filter
--takes a function that returns Bool and a list that function can affect
--	keep only the elements of a list which satisfy a boolean function with true
-- e.g filter (<5) [1,3,5,7,2] => [1,3,2] 






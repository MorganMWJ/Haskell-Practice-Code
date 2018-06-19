module Revision where
--list comprehensions
doubleList :: [Int] -> [Int]
doubleList ints = 
	[x*2 | x <- ints]--single generator
	
--multiple generators 
pairTwoLists :: [a] -> [b] -> [(a,b)]
pairTwoLists list1 list2 = 
	[ (x,y) | x <- list1, y <- list2 ]
	
--example from book
practice :: [Int] -> [Int]
practice list = 
	[2*n | n <- list, n>3] 
--will times item n by two if 'n' is an element of 'list' and is greater than 3 
--function only returns elements that fit all criteria
type Pair = (Int,Int)
addPairs :: [Pair] -> [Int]
addPairs pairList = 
	[(a+b) | (a,b) <- pairList]
--will add 'a' and 'b' if '(a,b)' is an element of pairList

--function to check whether all elements are less than 3
allLessThanThree :: [Int] -> Bool
allLessThanThree xs = (xs == [x | x <- xs, x<3])
--if given list is the same as one returned from list comp 
--where list comp returns all elements that are inside xs and less than 3 


count :: Char -> [Char] -> Int
count letter word = length [ x | x <- word, x==letter ] 
--only in final list if a element of word and the same as letter
--then return length of that produced list

--map is a list comprehension
map' :: (a -> b) -> [a] -> [b]
map' function listOfParameters = 
	[function parameter | parameter <- listOfParameters]

double :: [(String,Int,String)] -> [(String,Int,String)]
double list = [(f,q*2,u) | (f,q,u) <- list]

--[ 3 | x <- [1..3] ]
-- => [3,3,3]

--[ 'r' | u <- [1..4] ]
-- =>"rrrr"
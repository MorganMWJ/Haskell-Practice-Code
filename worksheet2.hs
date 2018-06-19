module Practice where

pairTeams teams = 
	[ (x,y) | x <- teams, y <- teams, x/=y]
	
square :: Int -> Int
square x = x*x

double :: Int -> Int 
double x = x*2

triple :: Int -> Int 
triple x = x*3


lessThanTen :: Int -> Bool
lessThanTen x = 
	if x<10 
		then True
		else False

startsWithA word 
	|head word == 'A' = True
	|otherwise = False
	
squaresLessThanTen :: [Int] -> [Int]
squaresLessThanTen list = filter lessThanTen [square x | x <- list] 
--filter is used because filter will return values from list comp that are true
--but map would not work because map would apply the function causing result to be a list of true and false
triplePos :: [Int] -> [Int]
triplePos list = filter (>(-1)) tripledList
		where
		tripledList = [x*3 | x <- list]--times x by 3 as long as x is an element of list

triplePos' :: [Int] -> [Int]		
triplePos' list = returnPositives (map (*3) list)
	where 
		returnPositives :: [Int] -> [Int]
		returnPositives xs = [x | x <- xs, x>(-1)]
	
	
	
	
	
	
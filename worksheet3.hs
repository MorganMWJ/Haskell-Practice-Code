member :: Eq a => a -> [a] -> Bool -- don't understand this type signature 'Eq a =>'
member y []     = False
member y (x:xs) = 
	if y == x 
		then True 
		else member y xs
		
beginning :: [a] -> [a]
beginning []     = []
beginning [x]    = []
beginning (y:x:[]) = y : []
beginning (y:x:xs) =  y : x : beginning xs
	--have an accumulator that adds up too length of list -1
	
at :: Int -> [a] -> a
at n []     = error "Talk Shit, Get Hit"
at 0 (x:xs) = x
at n (x:xs) = at (n-1) xs

interval :: Int -> Int -> [Int]
interval i j
	| i==j = [i] 
	| i<j  = i : interval (i+1) j
	| i>j  = i : interval (i-1) j
	
duplicate :: [a] -> [a]
duplicate []     =  []
duplicate [x]    = [x,x]-- not needed
duplicate (x:xs) = x : x : duplicate xs

between :: a -> [a] -> [a]
between element []  = []
between element [x] = [x]
between element (x:xs) = x : element : between element xs


beginnings :: [a] -> [[a]]
beginnings [] = [[]]



















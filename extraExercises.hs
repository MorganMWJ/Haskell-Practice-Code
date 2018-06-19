module Practice where

choose :: Int -> Int -> Int 
choose n m = div (factorial m) ((factorial n)*(factorial (m-n)))
	where
		factorial :: Int -> Int 
		factorial 0 = 1
		factorial x = x*factorial(x-1)
		
improveGuess :: Double -> Double -> Double 
improveGuess a x = 0.5 * (x + (a/x))

approxSqrt :: Double -> Double -> Double
approxSqrt previousGuess numberToRoot = 
	if similar previousGuess newGuess
		then newGuess
		else approxSqrt newGuess numberToRoot 
	where 	
		newGuess = improveGuess numberToRoot previousGuess
		similar x y = abs(x-y) < 0.01
		
--4
function :: Int -> Int -> (Int -> a) -> [a]
function m n f = if m>n
					then []
					else if m==n
							then [f n]
							else (f m) : function (m+1) n f 
							
							
--15
elemNum :: Int -> [Int] -> Int
elemNum n [] = 0
elemNum n (x:xs) =
	if x==n 
		then 1 + elemNum n xs 
		else elemNum n xs 
		
elemNum' :: Int -> [Int] -> Int
elemNum' n list = length ([ x | x <- list, x==n ])
		
elemNum'' :: Int -> [Int] -> Int
elemNum'' n list = length (filter (==n) list)

--10
averageThree :: Int -> Int -> Int -> Float
averageThree i1 i2 i3 = (sum (map fromIntegral [i1,i2,i3]))/3

--14
--the most general type signature for sing
sing :: a -> [a]
sing x = [x]

--7
final :: [a] -> a
final [] = error "No elements in list"
final [x] = x
final (x:xs) = final xs	

final' :: [a] -> a
final' [] = error "No elements in list"
final' list = (head (reverse list))

--9
justify :: Char -> Int -> String -> String
justify command spaces text = 
	case command of
		'l' -> text ++ (insert spaces)
		'c' -> (insert spaces) ++ text ++ (insert spaces)
		'r' -> (insert spaces) ++ text

insert :: Int -> String
insert 0 = ""
insert n = ' ':insert (n-1)


	
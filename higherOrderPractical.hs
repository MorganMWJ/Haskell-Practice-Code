module Morgan's where

thrice :: (a -> a) -> a -> a
thrice function x = function (function (function x))

thrice' :: (a -> a) -> a -> a
thrice' function = function . function . function 

trimSp :: String -> String 
trimSp = reverse . dropWhile isSpace . reverse . dropWhile isSpace
	where
		isSpace :: Char -> Bool
		isSpace char = 
			if char == ' '
				then True
				else False
				
diffs list = zipWith (-) list (tail list)

locations :: Eq a => a -> [a] -> [Int] --Eq a says that first a must be able to be equated
locations item list = 
	map fst (filter isItem (zip [0..] list))
		where 
			isItem (int, element) = 
				if element == item 
					then True 
					else False
					
splitter :: (a -> Bool) -> [a] -> ([a],[a])
splitter boolFunction list = 
	(takeWhile boolFunction list, dropWhile boolFunction list)

myLength xs = foldr function 0 xs
	where 
		function x r = 1 + r


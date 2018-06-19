module MOCK where

joinWords :: [String] -> String 
joinWords xs = 
	concat (map addSpace xs)
	where
		addSpace x = x ++ " "
		
smallest :: (Ord a) => [a] -> a
smallest [x] = x
smallest (x:xs) = if x<(smallest xs) 
						then x
						else smallest xs
						
data ComputerRoom = Delph | Orchard | B23 | PHYS112 | DSL deriving (Show)

roomSize :: ComputerRoom -> Int
roomSize B23 = 102
roomSize DSL = 20 
roomSize _   = 40 

isLarger :: ComputerRoom -> ComputerRoom -> Bool
isLarger r1 r2 = (roomSize r1) > (roomSize r2)

displayRoom :: ComputerRoom -> String
displayRoom x = show x ++ ": can seat " ++ show (roomSize x)

-------question 2-----------
addTwoInts :: Int -> Int -> Int 
addTwoInts x y = x+y

--c equivanlent
-- int addTwoInts (int x, int y) {
--		return x+y;
--	}


recDouble :: [Int] -> [Int]
recDouble [] = []
recDouble [x] = [x*2]
recDouble (x:xs) = x : recDouble xs 
data Cardnum = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten deriving(Ord, Eq, Show) 
data Value = Ace | Number Cardnum | Jack | Queen | King deriving(Ord, Eq, Show)

tester :: Value -> Value -> String
tester v1 v2 = if v1<v2 
					then (show v1) ++ " is before " ++ (show v2) ++ " in the ADT Value.(fist parameter is lesser)"
					else if v1>v2 
							then (show v2) ++ " is before " ++ (show v1) ++ " in the ADT Value.(fist parameter is greater)"
							else "Both are equal."
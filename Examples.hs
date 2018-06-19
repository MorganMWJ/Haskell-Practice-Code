module Revision where

-- The function to square an integer
sqrDoubleTriple :: Int -> Int
sqrDoubleTriple = triple . double . square where

square   :: Int -> Int
square x = x * x

	-- The function to double an integer

double   :: Int -> Int
double x = x + x 

triple :: Int -> Int
triple x = x*3

-- Some type synonyms

type Grade = Char
type Username = String

type ExamResult = (Username,Grade)

congradulate :: (Username,Grade) -> String
congradulate (user, grade) = 
	if grade == 'A' 
		then "well done " ++ user 
		else "we are Asians not Bsians!!!"
		

congrats :: ExamResult -> String
congrats (user, grade) 
	| grade == 'A' = "well done " ++ user
	| grade == 'a' = "well done " ++ user	
	| otherwise = "we are Asians not Bsians!!!"
		
type Firstname = String
type Surname = String

greet :: Firstname -> Surname -> String
greet f s = "Hello " ++ f ++ " " ++ s ++ ", how's life?"

type TeamScore    = (String,Int)
type MatchOutCome = (TeamScore, TeamScore)

winningTeam :: MatchOutCome -> String
winningTeam ((t1,s1), (t2,s2)) 
	|s1 > s2 = t1
	|otherwise = t2
	
winningTeam' :: MatchOutCome -> String--takes in a matchoutcome or two teamscore's which each are tuples of two items
winningTeam' (teamScore1, ts2) = 
	if (snd teamScore1)>(snd ts2) then fst teamScore1
	else fst ts2
	

goalDifference :: MatchOutCome -> Int
goalDifference (ts1,ts2)  
	| (snd ts1) > (snd ts2) = (snd ts1) - (snd ts2)
	| (snd ts1) < (snd ts2) = (snd ts2) - (snd ts1)
	| otherwise = 0

	
showTeamResult :: TeamScore -> String  
showTeamResult (team,score) =
	"In the end " ++ team ++ " scored " ++ show score ++ " goals." 
	--Int variable in a string show is needed
	
showTeamResult' :: TeamScore -> String
showTeamResult' teamscore = 
	"In the end " ++ fst teamscore ++ " scored " ++ show (snd teamscore) ++ " goals."


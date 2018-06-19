
greet :: String -> Char -> String
greet name lang
	|lang == 'e' = greetEnglish
	|lang == 'E' = greetEnglish
	|lang == 'f' = greetFrench
	|lang == 'F' = greetFrench
	|otherwise   = "We have no greeting in that language, sorry."
	where
		greetEnglish = "Hello, " ++ name
		greetFrench  = "Bonjour, " ++ name
		
------------------------------------------------------------------------
doubleTriple :: Int -> (Int,Int)
doubleTriple x = (x*2,x*3)

------------------------------------------------------------------------

type Grade = Char
type Username = String

type ExamResult = (Username,Grade)

higherScore :: ExamResult -> ExamResult -> Username
higherScore pupil1 pupil2 =
	if (snd pupil1)  < (snd pupil2) --'a' is lesser letter in storage of ASCII
		then fst pupil1
		else if (snd pupil1)  > (snd pupil2)
				then fst pupil2
				else "Both students have done equally well!" 
		
higherScore' :: ExamResult -> ExamResult -> Username
higherScore' (name1,grade1) (name2,grade2)
	|grade1 < grade2  = name1 ++ " did better than " ++ name2
	|grade1 > grade2  = name2 ++ " did better than " ++ name1
	|grade1 == grade2 = "Both students did equally well!" 
	--Username is just another word for String return type
	
------------------------------------------------------------------------

circumAndArea :: Float -> (Float,Float)
circumAndArea r = (circum,area)
	where
		circum = 2*r*pi
		area   = (r**2)*pi  
		
circumAndArea' :: Float -> (Float,Float)
circumAndArea' r = 
	let 
		circum = 2*r*pi
		area   = (r**2)*pi 
	in
		(circum,area)





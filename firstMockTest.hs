type Weight = Int
type Postcode = String
type FirstClass = Bool
type Parcel = (Weight,Postcode,FirstClass)

isHeavy :: Parcel -> Bool
isHeavy (w,p,s) = 
	if w>300
		then True
		else False
		
firstClassParcels :: [Parcel] -> [Parcel]
firstClassParcels ps = filter firstClass ps
	where
		firstClass :: Parcel -> Bool
		firstClass (w,p,f) = f -- can be done because FirstClass only takes in one parcel at a time
--I prefer using list comp 		
firstClassParcels' :: [Parcel] -> [Parcel]
firstClassParcels' list = 
	[x | x <- list, firstClass x]
	where
		firstClass :: Parcel -> Bool
		firstClass (w,p,f) = f -- can be done because FirstClass only takes in one parcel at a time	
		

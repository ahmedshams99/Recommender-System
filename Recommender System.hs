data Item=I [Char] deriving (Show,Eq)
data User=U [Char]	deriving (Show,Eq)
data Fractional a => Rating a = NoRating|R a deriving (Show,Eq)

contains a []=False
contains a (x:xs)=if(a==x) then True else contains a xs

dis :: Eq a => [a] -> [a]
dis []=[]
dis (x:xs)=if(contains x xs) then dis xs else [x]++dis xs

fromRatingsToItems :: Eq a => [(b,a,c)] -> [a]
fromRatingsToItems a=dis (frtiHelper a)
frtiHelper []=[]
frtiHelper ((_,b,_):xs)=[b]++frtiHelper xs

fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a]
fromRatingsToUsers a=dis (frtuHelper a)
frtuHelper []=[]
frtuHelper ((b,_,_):xs)=[b]++frtuHelper xs

hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating a b []=False
hasRating a b ((x,y,_):xs)=if(x==a && b==y) then True else hasRating a b xs

getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c
getRating a b []=error("No given rating")
getRating a b ((x,y,z):xs)=if(x==a && b==y) then z else getRating a b xs

getRatingClean a b []=NoRating
getRatingClean a b ((x,y,z):xs)=if(x==a && b==y) then (R z) else getRatingClean a b xs

formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]
formMatrixUser a [] c=[]
formMatrixUser a (b:bs) c=(getRatingClean a b c):formMatrixUser a bs c

formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]
formMatrix [] _ _=[]
formMatrix (x:xs) y z=(formMatrixUser x y z):formMatrix xs y z

numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b
numberRatingsGivenItem _ []=0
numberRatingsGivenItem x (y:ys) = if((y!!x)==NoRating) then 0+numberRatingsGivenItem x ys else 1+numberRatingsGivenItem x ys

differenceRatings :: Fractional a => Rating a -> Rating a -> a
differenceRatings NoRating _=0
differenceRatings _ NoRating=0
differenceRatings (R a) (R b)=a-b

matrixPairs :: Num a => a -> [(a,a)]
matrixPairs 0 =[]
matrixPairs a =(mphelper a 0 0)
mphelper a b c=if(b==(a-1) && c==(a-1)) then [(b,c)] else if(c==(a-1)) then [(b,c)]++(mphelper a (b+1) 0) else [(b,c)]++(mphelper a b (c+1))

dMatrix :: Fractional a => [[Rating a]] -> [a]
dMatrix r=dMatrixHelper1 r (matrixPairs x) where x =length(r!!0)
dMatrixHelper1 _ []  =[]
dMatrixHelper1 r ((a,b):xs) =[dMatrixHelper2 r a b] ++ dMatrixHelper1 r xs
dMatrixHelper2 [] _ _ =0
dMatrixHelper2 (x:xs) a b =differenceRatings (x!!a) (x!!b) + dMatrixHelper2 xs a b

freqMatrix :: (Num a, Fractional b) => [[Rating b]] -> [a]
freqMatrix r =freqMatrixHelper1 r (matrixPairs x) where x =length(r!!0)
freqMatrixHelper1 _ [] = []
freqMatrixHelper1 r ((a,b):xs) = [freqMatrixHelper2 r a b] ++ freqMatrixHelper1 r xs
freqMatrixHelper2 [] _ _=0
freqMatrixHelper2 (x:xs) a b= if(x!!a == NoRating || x!!b == NoRating ) then freqMatrixHelper2 xs a b else 1+freqMatrixHelper2 xs a b

diffFreqMatrix :: Fractional a => [[Rating a]] -> [a]
diffFreqMatrix r =diffFreqMatrixH (dMatrix r) (freqMatrix r)
diffFreqMatrixH [] []=[]
diffFreqMatrixH (x:xs) (y:ys) = if(y==0) then [0]++  diffFreqMatrixH xs ys 
					else [(x/y)]++  diffFreqMatrixH xs ys 


predict :: (Fractional a, Eq b, Eq c) => [(b,c,a)] -> Int -> Int -> a

predict x indexofuser indexofitem = (predictHelper indexofitem 0 (r!!indexofuser) diff) / (getz (r!!indexofuser))  where (r,diff)=(formMatrix (fromRatingsToUsers  x) (fromRatingsToItems x) x,diffFreqMatrix r) 




predictHelper unknownitem item r diff = if(item==length(r)) then 0
					else if(r!!item==NoRating) then predictHelper unknownitem (item+1) r diff
					else  (differenceRatings (r!!item) (R 0))+diff!!(unknownitem*(length(r))+item)+predictHelper unknownitem (item+1) r diff 

getz r = 1.0*(fromIntegral(getzHelper r))
getzHelper []=0
getzHelper (NoRating:xs)=getzHelper xs
getzHelper (_:xs)=1+getzHelper xs

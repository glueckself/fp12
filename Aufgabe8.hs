import Data.List

type InitialValue = Integer
type NumberOfRounds = Integer
type MaxRounds = Integer
type FinalValue = Integer
type TextRep = String
data Solution = Failure | Success (NumberOfRounds,FinalValue,TextRep) deriving (Eq, Show)

isPal :: Integer -> Bool
isPal num = let str = show num in
                str == (reverse str)

nextValue :: Integer -> Integer
nextValue num = let revNum = read(reverse(show num)) in
                    num+revNum

addRev' :: NumberOfRounds -> MaxRounds -> Integer -> Solution
addRev' round maxRounds value | round > maxRounds = Failure
                              | otherwise = if (isPal value)
                                               then Success(round,value,show value)
                                               else addRev' (round+1) maxRounds (nextValue value)

addRev :: InitialValue -> MaxRounds -> Solution
addRev inVal maxRounds = addRev' 0 (abs maxRounds) (abs inVal)



type Country = String
type Countries = [Country]
type TravelTime = Integer -- Travel time in minutes
data Connection = Air Country Country TravelTime
                  | Sea Country Country TravelTime
                  | Rail Country Country TravelTime
                  | Road Country Country TravelTime deriving (Eq,Ord,Show)
type Connections = [Connection]
data Itinerary = NoRoute | Route (Connections,TravelTime) deriving (Eq,Ord,Show)

type SimplConnections = [(Country,Country,TravelTime)]

simDb = [(Road "at" "hu" 10),
         (Road "hu" "sr" 20),
         (Road "at" "de" 10),
         (Road "de" "fr" 30),
         (Road "it" "at" 10),
         (Air "at" "at" 2),
         (Sea "it" "fr" 30),
         (Air "fr" "sr" 5)]
         
fst' (x,_,_) = x
snd' (_,x,_) = x
trd' (_,_,x) = x

simplCon :: Connection -> (Country,Country,TravelTime)
simplCon (Air c1 c2 t) = (c1,c2,t)
simplCon (Sea c1 c2 t) =  (c1,c2,t)
simplCon (Rail c1 c2 t) = (c1,c2,t)
simplCon (Road c1 c2 t) = (c1,c2,t)


expandList :: SimplConnections -> [Country] -> [Country]
expandList db crt = union [ snd' x | x <- db, elem (fst' x) crt ]
                    (union [ fst' x | x <- db, elem (snd' x) crt ] crt)

isRoute' :: SimplConnections -> [Country] -> Country -> Bool
isRoute' db crtList dst = if elem dst crtList
                             then True
                             else let newList = expandList db crtList in
                                  let lenN = length newList in
                                  let lenC = length crtList in
                                      if lenN > lenC
                                         then isRoute' db newList dst
                                         else False
                                         
isRoute :: Connections -> Country -> Country -> Bool
isRoute db start dest = isRoute' (map simplCon db) [start] dest

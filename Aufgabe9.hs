import Data.List

-- Beispiel 1
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

fst' (x,_,_) = x
snd' (_,x,_) = x
trd' (_,_,x) = x

-- Hilfsfunktion, um Daten vom Konstruktor unabhaengig zu machen
simplCon :: Connection -> (Country,Country,TravelTime)
simplCon (Air c1 c2 t) = (c1,c2,t)
simplCon (Sea c1 c2 t) =  (c1,c2,t)
simplCon (Rail c1 c2 t) = (c1,c2,t)
simplCon (Road c1 c2 t) = (c1,c2,t)

-- Hilfsfunktion, die beide Leander zurueckliefert
getCountries :: Connection -> Countries
getCountries e = [fst' elem,snd' elem]
                 where elem = simplCon e

-- Erzeugt eine Liste aus der Datenbank
flatDb :: Connections -> Countries
flatDb db = concatMap (\e -> getCountries e) db

-- Erweitert eine Liste um alle Nachbarn ihrer Elemente
expandList :: Connections -> Countries -> Countries
expandList fullDb crt = union [ snd' x | x <- db, elem (fst' x) crt ] (union [ fst' x | x <- db, elem (snd' x) crt ] crt)
                    where db = map simplCon fullDb

-- Sucht alle Nachbarn eines Landes
getAllNeighbours :: Connections -> Countries -> Countries
getAllNeighbours db crtList = let newList = expandList db crtList
                                  lenN = length newList
                                  lenC = length crtList in
                                  if lenN > lenC
                                     then getAllNeighbours db newList
                                     else crtList

yieldUnreachable :: Connections -> Country -> Countries
yieldUnreachable db cntry = nub (filter (\e -> notElem e (getAllNeighbours db [cntry])) (flatDb db))

-- Hilfsfunktion, die nur Rail oder Road verbindungen zurueckgibt
filterDb :: Connections -> Connections
filterDb db = filter rules db
              where rules (Air _ _ _) = False
                    rules (Sea _ _ _) = False
                    rules (Rail _ _ _) = True
                    rules (Road _ _ _) = True

yieldGroundReachable :: Connections -> Country -> Countries
yieldGroundReachable db cntry = getAllNeighbours (filterDb db) [cntry]

-- Beispiel 2
-- Hilfsfunktion, die die Position "unbedingt" zurueckliefert
getPos :: [Integer] -> Integer -> Integer
getPos lst x = let pos = elemIndex x lst in
                   case pos of
                    Just value -> (fromIntegral value)+1
                    Nothing -> 0

-- Hilfsfunktion, die die Zahlenliste nach dem jeweils das n-te Element ignoriert.
-- pos = Anzahl der vorherigen Elemente
luckyNumbers' :: [Integer] -> Integer -> [Integer]
luckyNumbers' (1 : xs) pos = 1 : luckyNumbers' xs (pos+1)
luckyNumbers' (p : xs) pos = p : (luckyNumbers' [x | x <- xs, (mod ((getPos xs x)+pos) p) > 0] (pos+1))

-- 1 soll nicht angezeigt werden, muss aber in der Liste beruecksichtigt werden
luckyNumbers :: [Integer]
luckyNumbers = delete 1 (luckyNumbers' [1,3..] 1)

-- takeWhile, da sonst Endlosschleife
isLuckyNumber :: Integer -> Bool
isLuckyNumber n = elem n (takeWhile (<=n) luckyNumbers)

-- siehe oben
yieldLuckyNumbers :: Integer -> Integer -> [Integer]
yieldLuckyNumbers min max = filter (\e -> (min <= e) ) (takeWhile (<=max) luckyNumbers)

isTwinLuckyNumber :: Integer -> Bool
isTwinLuckyNumber n = (isLuckyNumber n)&&( (isLuckyNumber (n+2))||(isLuckyNumber (n-2)) )


simDb = [(Road "at" "hu" 10),
         (Road "hu" "sr" 20),
         (Road "at" "de" 10),
        -- (Road "de" "fr" 30),
         (Road "it" "at" 10),
         (Sea "it" "fr" 30),
         (Air "fr" "sr" 5),
         (Air "usa" "ca" 5)]

import Data.List

-- Bsp. 1
-- "kgv" koennte "agv" benutzen, um dann nur das erste Element auszuwaehlen.
-- Da "agv" jedoch mehr Rechenaufwand hat und der bereich 0-unendlich waere
-- verwende ich hier den Zusammenhang zw. ggT und kgV
-- Alternativ waere eine Primfaktorzerlegung auch moeglich gewesen.

-- Hilfsfunktion ggt
ggt :: Integer -> Integer -> Integer
ggt m n 
      | n == 0 = m
      | otherwise = ggt n (mod m n)

-- kgv
kgv :: Integer -> Integer -> Integer
kgv m n = let a = abs(m) in
          let b = abs(n) in
                if (a == 0) then 0
                else if (b == 0) then 0
                     else div (a*b) (ggt a b)

-- Bsp. 2
-- Hilfsfunktion, die ein Vielfaches von a mit p <= v <= q bildet
vf :: Integer -> (Integer,Integer) -> [Integer]
vf a (p,q) = let p1 = if (p > 0) then div p a 
                                 else 1
                                 in
             let q1 = if (q > 0) then div q a 
                                 else 1
                                 in
                                     map (*a) [p1..q1]
-- agv
agv :: Integer -> Integer -> (Integer,Integer) -> [Integer]
agv m n (p,q) = let a = abs(m) in
                let b = abs(n) in
                      if (a == 0) then []
                      else if (b == 0) then []
                           else nub (intersect (vf a (p,q)) (vf b (p,q)))

-- Bsp. 3/4/5
type PassName = String
type FlightNumber = Integer
type PlaceOfDeparture = String
type Destination = String
type Airfare = Integer
type Database = [(PassName,FlightNumber,PlaceOfDeparture,Destination,Airfare)]

-- Bsp. 3

flights :: Database -> PassName -> [(FlightNumber,Airfare)]
flights db name = let result = filter (\(p,f,pd,d,a) -> p == name) db in
                    sort (map (\(p,f,pd,d,a) -> (f,a)) result)


-- pass2Dest
pass2Dest :: Database -> Destination -> [PassName]

pass2Dest db dest = []

-- mostValuedPass
mostValuedPass :: Database -> PlaceOfDeparture -> Destination -> ([PassName],Airfare)

mostValuedPass db pod dest = ([],0)

-- test database
db =  [ ("Anton",857,"Vienna","London",237),
        ("Berta",456,"Paris","Berlin",278),
        ("Anton",123,"Rome","London",417),
        ("Anton",109,"London","Berlin",237),
        ("Karla",888,"Vienna","Rome",350),
        ("Karla",832,"Rome","London",417),
        ("Berta",857,"Vienna","London",199),
        ("Karla",753,"Vienna","London",237) ]


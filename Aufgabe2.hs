-- Bsp. 1
-- Hilfsfunktion ggt
ggt :: Integer -> Integer -> Integer
ggt m n 
      | n == 0 = m
      | otherwise = ggt n (mod m n)

kgv :: Integer -> Integer -> Integer

kgv m n = let a = abs(m) in
          let b = abs(n) in
                if (a == 0) then 0
                else if (b == 0) then 0
                     else div (a*b) (ggt a b)

-- Bsp. 3/4/5

type PassName = String
type FlightNumber = Integer
type PlaceOfDeparture = String
type Destination = String
type Airfare = Integer
type Database = [(PassName,FlightNumber,PlaceOfDeparture,Destination,Airfare)]

flights :: Database -> PassName -> [(FlightNumber,Airfare)]

flights db name = []

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


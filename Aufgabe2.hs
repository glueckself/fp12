-- Bsp. 3/4/5

type PassName = String
type FlightsNumber = Integer
type PlaceOfDeparture = String
type Destination = String
type Airfare = Integer
type Database = [(PassName,FlightNumber,PlaceOfDeparture,Destination,Airfare)]

flights :: Database -> Passname [(FlightNumber,Airfare)]


pass2Dest :: Database -> Destination -> [PassName]

mostValuedPass :: Database -> PlaceOfDeparture -> Destination -> ([PassName],Airfare)

db =    ("Anton",857,"Vienna","London",237),
        ("Berta",456,"Paris","Berlin",278),
        ("Anton",123,"Rome","London",417),
        ("Anton",109,"London","Berlin",237),
        ("Karla",888,"Vienna","Rome",350),
        ("Karla",832,"Rome","London",417),
        ("Berta",857,"Vienna","London",199),
        ("Karla",753,"Vienna","London",237) ]


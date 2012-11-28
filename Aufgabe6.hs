import Data.List (genericReplicate)

-- Beispiel 1

data Nat = Z | S Nat

instance Show Nat where
   show a = toRoman a

-- Aus Aufgabe 4 (Hilfsfunktionen)
plusNat :: Nat -> Nat -> Nat
plusNat Z b = b
plusNat (S a) b = S (plusNat a b)

minusNat :: Nat -> Nat -> Nat
minusNat Z b = Z
minusNat a Z = a
minusNat (S a) (S b) = minusNat a b

timesNat' :: Nat -> Nat -> Nat -> Nat
timesNat' Z b r = r
timesNat' (S a) b r = timesNat' a b (plusNat b r)

timesNat :: Nat -> Nat -> Nat
timesNat a b = timesNat' a b Z

divNatFloor :: Nat -> Nat -> Nat -> Nat
divNatFloor Z b r = r
divNatFloor a b r | (grEqNat a b) = divNatFloor (minusNat a b) b (S r)
                  | otherwise = r

divNat :: Nat -> Nat -> Nat
divNat a Z = error "Invalid argument"
divNat a b = divNatFloor a b Z

grEqNat :: Nat -> Nat -> Bool
grEqNat a b = (eqNat a b) || (grNat a b)

modNat :: Nat -> Nat -> Nat
modNat a b = minusNat a (timesNat (divNat a b) b)

grNat :: Nat -> Nat -> Bool
grNat Z Z = False
grNat a Z = True
grNat Z b = False
grNat (S a) (S b) = grNat a b

eqNat :: Nat -> Nat -> Bool
eqNat Z Z = True
eqNat a Z = False
eqNat Z b = False
eqNat (S a) (S b) = eqNat a b

-- Von und zu Integer
intToNat :: Integer -> Nat
intToNat 0 = Z
intToNat n = (S (intToNat(n-1)))

natToInt :: Nat -> Integer
natToInt Z = 0
natToInt (S a) = 1 + natToInt a

-- Um mir das schreiben zu vereinfachen, verwende ich intToNat und Integer
convMap = [((intToNat 1000),"M"),  ((intToNat 999), "IM"), ((intToNat 990), "XM"), ((intToNat 900), "CM"),
           ((intToNat 500), "D"),  ((intToNat 499), "ID"), ((intToNat 490), "XD"), ((intToNat 400), "CD"),
           ((intToNat 100), "C"),  ((intToNat 99),  "IC"), ((intToNat 90),  "XC"),
           ((intToNat 50) , "L"),  ((intToNat 49),  "IL"), ((intToNat 40),  "XL"),
           ((intToNat 10),  "X"),  ((intToNat 9),   "IX"),
           ((intToNat 5),   "V"),  ((intToNat 4),   "IV"),
           ((intToNat 1),   "I")]

-- Idee von http://codereview.stackexchange.com/questions/5550/obvious-design-flaws-in-haskell-code-convert-to-roman-numerals,
-- umgebaut damit direkt auf Nat gearbeitet wird.
-- Da Nat keine Instanz von Integer ist, muss natToInt fuer genericReplicate verwendet werden
toRoman :: Nat -> String
toRoman Z = ""
toRoman x = snd (foldl f (x,[]) convMap)
    where f (n,s) (rn, rs) = (l, s++concat(genericReplicate (natToInt k) rs))
            where (k,l) = (divNat n rn, modNat n rn)



-- Beispiel 2

data RatNumbers = Rat Numerator Denominator
type Numerator = Nat
type Denominator = Nat

type NegaBinary = String

-- Beispiel 2a

instance Show RatNumbers where
    show (Rat a b) = (natToNb a "0")++"/"++(natToNb b "0")

natToNb :: Nat -> NegaBinary -> NegaBinary
natToNb Z b = b
natToNb (S n) b = natToNb n (nbIncr b)

-- Hifsfunktionen aus HaskellLive
cutZeros :: String -> String
cutZeros str
   | null conv = "0"
   | otherwise = conv
   where
      conv = dropWhile (=='0') str 

nbIncr :: String -> String
nbIncr z = cutZeros $ reverse $ incr2 (reverse z) 

nbDecr :: NegaBinary -> NegaBinary
nbDecr z = cutZeros $ reverse $ decr2 (reverse z)

decr2 :: NegaBinary -> NegaBinary
decr2 [] = "11"
decr2 ('1':x) = '0' : x
decr2 ('0':x) = '1' : (incr2 x)

incr2 :: NegaBinary -> NegaBinary
incr2 [] = "1"
incr2 ('0':x) = '1' : x
incr2 ('1':x) = '0' : (decr2 x)

-- Beispiel 2b
newtype NatP = NP (Nat,Nat) deriving Show
class Nf a where
    t2nf :: a -> a


    

data Nat = Z | S Nat
type NatPair = (Nat, Nat)

instance Show Nat where
   show a = reverse(cleanRom(convToRom a))

convToRom :: Nat -> String
convToRom Z = ""
convToRom (S a) = "I"++convToRom a

cleanRomI :: String -> String
cleanRomI "" = ""
cleanRomI ('I':'I':'I':'I':'I':num) = cleanRom("V"++(cleanRomI (num)))
cleanRomI ('I':'I':'I':'I':num) = cleanRom("VI"++(cleanRomI num))
cleanRomI num = num



intToNat :: Integer -> Nat
intToNat 0 = Z
intToNat n = (S (intToNat(n-1)))

testnum :: Integer -> Nat
testnum x = intToNat x

testroman = zip3 (map (\(x,y) -> (show $ intToNat x) == y) nums) (map snd nums) (map fst nums)
nums =
  [(19, "XIX"),
  (39, "XXXIX"),
  (40, "XL"),
  (49, "IL"),
  (84, "LXXXIV"),
  (89, "LXXXIX"),
  (98, "XCVIII"),
  (90, "XC"),
  (99, "IC"),
  (400, "CD"),
  (490, "XD"),
  (499, "ID"),
  (900, "CM"),
  (990, "XM"),
  (999, "IM"),
  (889, "DCCCLXXXIX"),
  (1899, "MDCCCIC"),
  (5884, "MMMMMDCCCLXXXIV"),
  (5999, "MMMMMIM"),
  (95, "XCV"),
  (1998, "MXMVIII")] 

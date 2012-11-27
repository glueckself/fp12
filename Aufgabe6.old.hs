data Nat = Z | S Nat
type NatPair = (Nat, Nat)

instance Show Nat where
    show a = cleanRom $ convToRom a


convToRom :: Nat -> String
convToRom Z = ""
convToRom (S a) = "I"++convToRom a

cleanRomI :: String -> String
cleanRomI ('I':'I':'I':'I':'I':num) = cleanRom("V"++cleanRomI num) -- (1)
cleanRomI ('I':'I':'I':'I':num) = cleanRom("IV"++cleanRomI num) -- (1)
cleanRomI num = num

cleanRomV :: String -> String
cleanRomV "" = ""
cleanRomV ('V':'V':num) = cleanRom("X"++cleanRomV num) -- (2)
cleanRomV ('V':'I':'V':num) = cleanRom("IX"++cleanRomV num) -- subtr
cleanRomV num = num

cleanRomX :: String -> String
cleanRomX "" = ""
cleanRomX ('X':'X':'X':'X':'X':num) = cleanRom("L"++cleanRomX num) -- (1)
cleanRomX ('X':'X':'X':'X':num) = cleanRom("XL"++cleanRomX num) -- (1)
cleanRomX num = num

cleanRomL :: String -> String
cleanRomL "" = ""
cleanRomL ('L':'L':num) = cleanRom("C"++cleanRomL num) -- (2)
--cleanRomL ('X':'L':'I':'X':num) = cleanRom("IL" ++ cleanRomL num)
cleanRomL num = num

cleanRomC :: String -> String
cleanRomC ('C':'C':'C':'C':'C':num) = cleanRom("D"++cleanRomC num)
cleanRomC ('C':'C':'C':'C':num) = cleanRom("CD"++cleanRomC num)
cleanRomC ('L':'X':'L':num) = cleanRom("XC"++cleanRomC num)
cleanRomC num = num

cleanRomD :: String -> String
cleanRomD ('D':'D':num) = cleanRom("M"++cleanRomD num)
cleanRomD ('X':'C':'I':'X':num) = cleanRom("IC"++cleanRomD num)
cleanRomD ('C':'D':'X':'C':num) = cleanRom("XD"++cleanRomD num)
cleanRomD num = num

cleanRomM :: String -> String
cleanRomM "" = ""
cleanRomM ('X':'D':'I':'X':num) = cleanRom("ID"++cleanRomM num)
cleanRomM ('D':'C':'D':num) = cleanRom("CM"++cleanRomM num)
cleanRomM('M':num) = "M"++cleanRomM num
cleanRomM num = num

cleanRom :: String -> String
cleanRom "" = ""
cleanRom ('X':'M':'I':'X':num) = cleanRom("IM" ++ cleanRom num)
cleanRom ('C':'M':'X':'C':num) = cleanRom("XM" ++ cleanRom num)
cleanRom ('L':'X':'L':'I':'X':num) = cleanRom("IM"++cleanRom num)
cleanRom num = cleanRomM $ cleanRomD $ cleanRomC $ cleanRomL $ cleanRomX $ cleanRomV $ cleanRomI num

intToNat :: Integer -> Nat
intToNat 0 = Z
intToNat n = (S (intToNat(n-1)))

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

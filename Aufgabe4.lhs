Typendefinition:

> data Nat = Z | S Nat deriving Show
> type NatPair = (Nat, Nat)

== Plus ==

> plusNat :: Nat -> Nat -> Nat
> plusNat Z b = b
> plusNat (S a) b = S (plusNat a b)


== Minus ==

> minusNat :: Nat -> Nat -> Nat
> minusNat Z b = Z
> minusNat a Z = a
> minusNat (S a) (S b) = minusNat a b


== Times ==

> timesNat' :: Nat -> Nat -> Nat -> Nat
> timesNat' Z b r = r
> timesNat' (S a) b r = timesNat' a b (plusNat b r)

> timesNat :: Nat -> Nat -> Nat
> timesNat a b = timesNat' a b Z

== Div ==

> divNatFloor :: Nat -> Nat -> Nat -> Nat
> divNatFloor Z b r = r
> divNatFloor a b r | (grEqNat a b) = divNatFloor (minusNat a b) b (S r)
>                   | otherwise = r

> divNatCeil :: Nat -> Nat -> Nat -> Nat
> divNatCeil Z b r = r
> divNatCeil a b r = divNatCeil (minusNat a b) b (S r)

> divNat :: Nat -> Nat -> Nat
> divNat a Z = error "Invalid argument"
> divNat a b = divNatFloor a b Z

== Mod ==

> modNat :: Nat -> Nat -> Nat
> modNat a b = minusNat a (timesNat (divNat a b) b)

== Eq ==

> eqNat :: Nat -> Nat -> Bool
> eqNat Z Z = True
> eqNat a Z = False
> eqNat Z b = False
> eqNat (S a) (S b) = eqNat a b

> grNat :: Nat -> Nat -> Bool
> grNat Z Z = False
> grNat a Z = True
> grNat Z b = False
> grNat (S a) (S b) = grNat a b

> leNat :: Nat -> Nat -> Bool
> leNat Z Z = False
> leNat a Z = False
> leNat Z b = True
> leNat (S a) (S b) = leNat a b

> grEqNat :: Nat -> Nat -> Bool
> grEqNat a b = (eqNat a b) || (grNat a b)

> leEqNat :: Nat -> Nat -> Bool
> leEqNat a b = (eqNat a b) || (leNat a b)

> mkCan :: NatPair -> NatPair
> mkCan (Z,b) = (Z,b)
> mkCan (a,Z) = (a,Z)
> mkCan ((S a),(S b)) = mkCan (a,b)

> plusNP :: NatPair -> NatPair -> NatPair
> plusNP (a1,Z) (b1,Z) = (plusNat a1 b1,Z)
> plusNP (Z,a2) (Z,b2) = (Z,plusNat a2 b2)
> plusNP (a1,Z) (Z,b2) = if grEqNat a1 b2
>                           then (minusNat a1 b2,Z)
>                           else (Z,minusNat b2 a1)
> plusNP (Z,a2) (b1,Z) = if grEqNat b1 a2
>                           then (minusNat b1 a2,Z)
>                           else (Z,minusNat a2 b1)
> plusNP a b = let a' = mkCan a
>                  b' = mkCan b
>              in plusNP a' b'

> minusNP :: NatPair -> NatPair -> NatPair
> minusNP a (b1,b2) = plusNP a (b2,b1)

> timesNP :: NatPair -> NatPair -> NatPair
> timesNP (a1,Z) (b1,Z) = (timesNat a1 b1, Z)
> timesNP (Z,a2) (Z,b2) = (timesNat a2 b2, Z)
> timesNP (a1,Z) (Z,b2) = (Z,timesNat a1 b2)
> timesNP (Z,a2) (b1,Z) = (Z,timesNat a2 b1)
> timesNP a b = let a' = mkCan a
>                   b' = mkCan b
>               in timesNP a' b'

> divNP :: NatPair -> NatPair -> NatPair
> divNP _ (Z,Z) = error "invalid argument"
> divNP (a1,Z) (b1,Z) = (divNatFloor a1 b1 Z, Z)
> divNP (Z,a2) (Z,b2) = (divNatFloor a2 b2 Z, Z)
> divNP (a1,Z) (Z,b2) = (Z,divNatCeil a1 b2 Z)
> divNP (Z,a2) (b1,Z) = (Z,divNatCeil a2 b1 Z)
> divNP a b = let a' = mkCan a
>                 b' = mkCan b
>             in divNP a' b'

> modNP :: NatPair -> NatPair -> NatPair
> modNP a b = minusNP a (timesNP (divNP a b) b)

> eqNP :: NatPair -> NatPair -> Bool
> eqNP a b = let (a1,a2) = mkCan a
>                (b1,b2) = mkCan b
>            in (eqNat a1 b1) && (eqNat a2 b2)

> grNP :: NatPair -> NatPair -> Bool
> grNP (a1,Z) (b1,Z) = grNat a1 b1
> grNP (a1,Z) (Z,b2) = True
> grNP (Z,a2) (b1,Z) = False
> grNP (Z,a2) (Z,b2) = leNat a2 b2
> grNP a b = let a' = mkCan a
>                b' = mkCan b
>            in grNP a' b'

> leNP :: NatPair -> NatPair -> Bool
> leNP (a1,Z) (b1,Z) = leNat a1 b1
> leNP (a1,Z) (Z,b2) = False
> leNP (Z,a2) (b1,Z) = True
> leNP (Z,a2) (Z,b2) = grNat a2 b2
> leNP a b = let a' = mkCan a
>                b' = mkCan b
>            in leNP a' b'

> grEqNP :: NatPair -> NatPair -> Bool
> grEqNP a b = (eqNP a b) || (grNP a b)

> leEqNP :: NatPair -> NatPair -> Bool
> leEqNP a b = (eqNP a b) || (leNP a b)

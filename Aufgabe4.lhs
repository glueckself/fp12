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

> divNat' :: Nat -> Nat -> Nat -> Nat
> divNat' Z b r = r
> divNat' a b r | (grEqNat a b) = divNat' (minusNat a b) b (S r)
>               | otherwise = r

> divNat :: Nat -> Nat -> Nat
> divNat a Z = error "Invalid argument"
> divNat a b = divNat' a b Z

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


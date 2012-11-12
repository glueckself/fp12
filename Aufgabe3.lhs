== Imports und Typendefinition ==

> import Data.List
> import Numeric
> import Data.Char
> type NegaBinary = String
> type NegaBit = Char

== Beispiel 1 ==
Hilfsfunktion um Null am Anfang zu entfernen

> remNull :: NegaBinary -> NegaBinary
> remNull nb | nb == [] = "0"
>            | otherwise = let elem = head nb in
>                              if elem == '1' then nb
>                              else remNull (tail nb)

> extract :: String -> NegaBinary
> extract str = remNull (filter (\s -> (s == '1' || s == '0')) str)

== Beispiel 2/3 ==
=== Hilfsfunktionen === 

Diese Funktion addiert a+b, mit entsprechender beruecksichtung vom Carry-Bit

> nbAddBit :: NegaBit -> NegaBit -> Int -> (NegaBit,Int)
> nbAddBit a b c = let aI = read [a] 
>                      bI = read [b]
>                  in case aI+bI+c of  0    -> ('0',0)
>                                      1    -> ('1',0)
>                                      2    -> ('0',-1)
>                                      3    -> ('1',-1)
>                                      (-1) -> ('1',1)
>                                      (-2) -> ('0',1)

Return entweder ein Element aus der Liste oder '0'

> nbGetItem :: NegaBinary -> Int -> NegaBit
> nbGetItem nb i = if i < length nb then nb !! i
>                                   else '0'

Addiert 2 NegaBits aus nbA und nbB an der Positios pos (mit Carry-Bit):

> nbAddHelper :: NegaBinary -> NegaBinary -> Int -> Int -> NegaBinary
> nbAddHelper nbA nbB pos carry = if max ((length nbA) - pos) ((length nbB) - pos) == 0
>                                    then case carry of 0  -> "0"
>                                                       1  -> "1"
>                                                       -1 -> "11"
>                                    else let result = nbAddBit (nbGetItem nbA pos) (nbGetItem nbB pos) carry in
>                                         ((fst result):(nbAddHelper nbA nbB (pos+1) (snd result)))

=== Beispiel 2 ===

> nbIncr :: NegaBinary -> NegaBinary
> nbIncr nb = remNull (reverse (nbAddHelper (reverse nb) "1" 0 0 ))

=== Beispiel 3 ===

> nbDecr :: NegaBinary -> NegaBinary
> nbDecr nb = remNull (reverse (nbAddHelper (reverse nb) "11" 0 0))


== Beispiel 4 ==

=== Hilfsfunktionen ===

Inkrementiert nb i mal.

> nbAbsHelperPos :: NegaBinary -> Int -> NegaBinary
> nbAbsHelperPos nb i = if i == 0
>                          then nb
>                          else nbAbsHelperPos (nbIncr nb) (i-1)

Zaehlt wie oft nb inkrementiert werden muss, bis es "0" ist und laesst dann nb so oft inkrementieren.

> nbAbsHelperNeg :: NegaBinary -> Int -> NegaBinary
> nbAbsHelperNeg nb i = if nb == "0"
>                          then nbAbsHelperPos nb i
>                          else nbAbsHelperNeg (nbIncr nb) (i+1)
                     
=== Beispiel 4 ===

> nbAbs :: NegaBinary -> NegaBinary
> nbAbs nb | mod (length nb) 2 == 1 = nb
>          | otherwise = nbAbsHelperNeg nb 0


== Beispiel 5/6 ==

=== Hilfsfunktionen ===
Diese Funktion verwendet leasst ueber countFunc count bis "0" ziehlen und wendet entsprechend oft calcFunc auf nb an.
ACHTUNG: countFunc muss so gewaehlt werden, dass count irgendwann "0" wird, sonst returnt diese Funktion nie!

> nbPlusHelper :: NegaBinary -> NegaBinary -> (NegaBinary -> NegaBinary) -> (NegaBinary -> NegaBinary) -> NegaBinary -> NegaBinary
> nbPlusHelper nb count countFunc calcFunc compval = if count == compval
>                                                       then nb
>                                                       else nbPlusHelper (calcFunc nb) (countFunc count) countFunc calcFunc compval
                                           
=== Bespiel 5 ===
Ueber das die Laenge von NegaBinary laesst sich das Vorzeichen bestimmen.
Davon abhaengig, wird nb2 von nbPlusHelper solange inkrementiert/dekrementiert bis es "0" ist.
Ensprechend oft wird nbIncr/nbDecr auf nb1 aufgerufen.

> nbPlus :: NegaBinary -> NegaBinary -> NegaBinary
> nbPlus nb1 nb2 | mod (length nb2) 2 == 0 = nbPlusHelper nb1 nb2 nbIncr nbDecr "0"
>                | otherwise = nbPlusHelper nb1 nb2 nbDecr nbIncr "0"


Anm.: Alternative (einfachere) Imlementierung:

> nbPlus' :: NegaBinary -> NegaBinary -> NegaBinary
> nbPlus' nb1 nb2 = remNull (reverse (nbAddHelper (reverse nb1) (reverse nb2) 0 0))

=== Beispiel 6 ===
Abhaengig von Vorzeichen, wird entweder nb1 oder nb2 als Multiplikator verwendet.
Sind beide Negativ, so wird der Betrag von ihnen fuer die Berechnung verwendet
(-4*-3=4*3=12)
Da nbPlusHelper bereits die Zaehlfunktion implementiert wird diese Funktion zum
Zahlen der noetigen Additionen verwendet.
Als "Trick" wird statt nbIncr/nbDecr jedoch eine lambda-Funktion mit der schnelleren
nbPlus'-Implementierung zur Addition verwendet.

> nbTimes :: NegaBinary -> NegaBinary -> NegaBinary
> nbTimes nb1 nb2 
>    | nb1 == "0" = "0"
>    | nb2 == "0" = "0"
>    | (mod (length nb2) 2 == 0) && (mod (length nb1) 2 == 1) = 
>        nbPlusHelper nb2 nb1 nbDecr (\nb -> nbPlus' nb2 nb) "1"
>    | (mod (length nb1) 2 == 0) && (mod (length nb2) 2 == 0) =
>        let nb1' = nbAbs nb1
>            nb2' = nbAbs nb2
>        in nbPlusHelper nb1' nb2' nbDecr (\nb -> nbPlus' nb1' nb) "1"
>    | otherwise = nbPlusHelper nb1 nb2 nbDecr (\nb -> nbPlus' nb1 nb) "1"


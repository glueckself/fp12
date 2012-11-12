Imports und Typendefinition

> import Data.List
> import Numeric
> import Data.Char
> type NegaBinary = String
> type NegaBit = Char

Beispiel 1:
Hilfsfunktion um Null am Anfang zu entfernen

> remNull :: NegaBinary -> NegaBinary
> remNull nb | nb == [] = "0"
>            | otherwise = let elem = head nb in
>                              if elem == '1' then nb
>                              else remNull (tail nb)

> extract :: String -> NegaBinary
> extract str = remNull (filter (\s -> (s == '1' || s == '0')) str)

Beispiel 2:
Hilfsfunktion (angelehnt an den binaeren Addierer):

> nbAddBit :: NegaBit -> NegaBit -> Int -> (NegaBit,Int)
> nbAddBit a b c = let aI = read [a] 
>                      bI = read [b]
>                  in case aI+bI+c of  0    -> ('0',0)
>                                      1    -> ('1',0)
>                                      2    -> ('0',-1)
>                                      3    -> ('1',-1)
>                                      (-1) -> ('1',1)
>                                      (-2) -> ('0',1)

> nbGetItem :: NegaBinary -> Int -> NegaBit
> nbGetItem nb i = if i < length nb then nb !! i
>                                   else '0'

> nbConvertToNb :: Int -> NegaBinary
> nbConvertToNb x
>                | x == 0 = "0"
>                | x == 1 = "1"
>                | otherwise = "error"

Addiert 2 NegaBinaries, ab Int, mit Carry NB

> nbAddHelper :: NegaBinary -> NegaBinary -> Int -> Int -> NegaBinary
> nbAddHelper nbA nbB pos carry = if max ((length nbA) - pos) ((length nbB) - pos) == 0
>                                    then case carry of 0  -> "0"
>                                                       1  -> "1"
>                                                       -1 -> "11"
>                                    else let result = nbAddBit (nbGetItem nbA pos) (nbGetItem nbB pos) carry in
>                                         ((fst result):(nbAddHelper nbA nbB (pos+1) (snd result)))

Da ich sehr lange gebraucht habe, NegaBinary zu verstehen

> nbIncr :: NegaBinary -> NegaBinary
> nbIncr nb = remNull (reverse (nbAddHelper (reverse nb) "1" 0 0 ))

> nbDecr :: NegaBinary -> NegaBinary
> nbDecr nb = remNull (reverse (nbAddHelper (reverse nb) "11" 0 0))

> nbAbsHelperPos :: NegaBinary -> Int -> NegaBinary
> nbAbsHelperPos nb i = if i == 0
>                          then nb
>                          else nbAbsHelperPos (nbIncr nb) (i-1)

> nbAbsHelperNeg :: NegaBinary -> Int -> NegaBinary
> nbAbsHelperNeg nb i = if nb == "0"
>                          then nbAbsHelperPos nb i
>                          else nbAbsHelperNeg (nbIncr nb) (i+1)
                     

> nbAbs :: NegaBinary -> NegaBinary
> nbAbs nb | mod (length nb) 2 == 1 = nb
>          | otherwise = nbAbsHelperNeg nb 0

nbPlus :: NegaBinary -> NegaBinary -> NegaBinary

nbPlus nb1 nb2 = remNull (reverse (nbAddHelper (reverse nb1) (reverse nb2) 0 0))



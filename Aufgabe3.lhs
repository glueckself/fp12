Imports und Typendefinition

> import Data.List
> import Numeric
> import Data.Char
> type NegaBinary = String

Beispiel 1:
Hilfsfunktion um Null am Anfang zu entfernen

> remNull :: NegaBinary -> NegaBinary
> remNull nb = let elem = head nb in
>               if elem == '1' then nb
>               else remNull (tail nb)

> extract :: String -> NegaBinary
> extract str = remNull (filter (\s -> (s == '1' || s == '0')) str)

Beispiel 2:
Hilfsfunktion (angelehnt an den binaeren Addierer):

> nbAddBit :: NegaBinary -> NegaBinary -> NegaBinary -> (NegaBinary,NegaBinary)
> nbAddBit a b c = let aI = read a 
>                      bI = read b
>                      cI = read c
>                  in case aI+bI+cI of 0    -> ("0","0")
>                                      1    -> ("1","0")
>                                      2    -> ("0","-1")
>                                      3    -> ("1","-1")
>                                      (-1) -> ("1","1")
>                                      (-2) -> ("0","1")

nbIncr :: NegaBinary -> NegaBinary

nbAdd :: NegaBinary -> NegaBinary -> NegaBinary
nbAdd nb1 nb2

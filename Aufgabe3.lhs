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

 nbAdd :: NegaBinary -> NegaBinary -> NegaBinary -> (NegaBinary,NegaBinary)
 nbAdd a b c =

nbIncr :: NegaBinary -> NegaBinary

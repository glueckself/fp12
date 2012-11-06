Das ist ein super tolles Haskell-Script.

> import Data.List
> type NegaBinary = String

Deklaration:

> extract :: String -> NegaBinary

Definition:

> extract str = filter (\s -> (s == '1' || s == '0')) str

So, und jetzt werd ichs ausprobieren.


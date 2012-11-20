import Data.List

-- Definitionen
data Tree = Null | Tree Label Tree Tree deriving (Eq,Show)
type Label = Integer

-- Bsp. 1
-- Patern matching damit alle Knoten bearbeitet werden koennen
tmap :: (Label -> Label) -> Tree -> Tree
tmap f Null = Null
tmap f (Tree i a Null) = Tree (f i) (tmap f a) Null
tmap f (Tree i Null b) = Tree (f i) Null (tmap f b)
tmap f (Tree i a b) = Tree (f i) (tmap f a) (tmap f b)

tzw :: (Label -> Label -> Label) -> Tree -> Tree -> Tree
tzw f Null b = Null
tzw f a Null = Null
tzw f (Tree i1 Null b1) (Tree i2 Null b2) = Tree (f i1 i2) Null (tzw f b1 b2)
tzw f (Tree i1 a1 Null) (Tree i2 a2 Null) = Tree (f i1 i2) (tzw f a1 a2) Null
tzw f (Tree i1 a1 b1) (Tree i2 a2 b2) = Tree (f i1 i2) (tzw f a1 a2) (tzw f b1 b2)

tfold :: (Label -> Label -> Label -> Label) -> Label -> Tree -> Label
tfold f v Null = v
tfold f v (Tree i a b) = f i (tfold f v a) (tfold f v b)

-- Bsp. 2
type ErdosNumber = Integer
data Scientist = Sc Initial SurName
type Initial = Char
type SurName = String
type Author = Scientist
newtype Database = Db [([Author],PaperTitle)]
type PaperTitle = String

-- Hilfsfunktionen
-- Vergleicht 2 Scientists (da sie nicht Eq verwenden)
compareSc :: Scientist -> Scientist -> Bool
compareSc (Sc i1 s1) (Sc i2 s2) = (i1 == i2) && (s1 == s2)

-- Ueberprueft, ob ein Scientist in einer Liste vorhanden ist
elemSc :: [Scientist] -> Scientist -> Bool
elemSc [] _ = False
elemSc (e:scl) sc = if compareSc e sc then True
                    else elemSc scl sc

-- Wenn ein Scientist e in der Liste sc vorhanden ist,
-- dann kann diese Liste verwendet werden
mkList' :: ([Author],PaperTitle) -> [Scientist] -> [Scientist]
mkList' _ [] = []
mkList' (sc,p) (e:lst) = if (elemSc sc e) then sc
                         else mkList' (sc,p) lst

-- Erzeugt eine "Oebermenge" (nicht sicher ob das der richtige Begriff ist)
-- der Liste von Scientists aus der Datenbank
mkList :: Database -> [Scientist] -> [Scientist]
mkList (Db []) _ = []
mkList (Db (e:db)) lst = unionBy compareSc (mkList' e lst) (mkList (Db db) lst)

-- Zaeht die Erdos-Nummer fuer jede neue Liste
checkList :: Database -> Scientist -> [Scientist] -> ErdosNumber -> ErdosNumber
checkList db sc lst ret = if (elemSc lst sc) then ret
                          else let nextList = mkList db lst in
                                   if ((length nextList) > (length lst)) then checkList db sc (mkList db lst) (ret+1)
                                   else -1

erdosNum :: Database -> Scientist -> ErdosNumber
erdosNum db (Sc 'P' "Erdos") = 0
erdosNum db sc = checkList db sc [(Sc 'P' "Erdos")] 0


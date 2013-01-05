import Data.List
import Control.Monad

type Movie =  (Title,Regisseur,MainActors,ReleaseDate,Genre,SalesPrice)
type Title = String
type Regisseur = String
type Actor = String
type MainActors = [Actor]
type ReleaseDate = Int
data Genre = Thriller | Fantasy | ScienceFiction | Comedy deriving (Eq,Ord,Show)
type SalesPrice = Int
type Database = [Movie]

instance (Show t, Show r, Show m, Show d, Show g, Show s) => Show (t,r,m,d,g,s) where
    show (t,r,m,d,g,s) = "("++(show t)++","++(show r)++","++(show m)++","
                         ++(show d)++","++(show g)++","++(show s)++")"

-- Hilfsfunktion: Vergleicht 2 Filme, True wenn gleich, False wenn nicht
compareMovie :: Movie -> Movie -> Bool
compareMovie (t1,r1,m1,d1,g1,s1) (t2,r2,m2,d2,g2,s2) = if (t1 == t2) &&
                                                          (r1 == r2) &&
                                                          (all (\e -> elem e m1) m2) &&
                                                          (d1 == d2) &&
                                                          (g1 == g2) &&
                                                          (s1 == s2) then True
                                                        else False

rm_dup :: Database -> Database
rm_dup db = nubBy compareMovie db

get_rtd :: Database -> [(Regisseur,Title,ReleaseDate)]
get_rtd db = map (\(t,r,_,d,_,_) -> (r,t,d)) db

get_rtg :: Database -> [(Regisseur,Title,Genre)]
get_rtg db = map (\(t,r,_,_,g,_) -> (r,t,g)) db

get_tad :: Database -> ReleaseDate -> [(Title,MainActors,ReleaseDate)]
get_tad db d = filter (\(_,_,d1) -> d1 /= d) (map (\(t,_,a,d,_,_) -> (t,a,d)) db)

-- Hilfsfunktion ob Actor in Movie
is_actor :: Actor -> Movie -> Bool
is_actor a (_,_,m,_,_,_) = if elem a m then True
                           else False

get_atr :: Database -> Actor -> [(Actor,Title,ReleaseDate)]
get_atr db a = map (\(t,_,_,d,_,_) -> (a,t,d)) (filter (is_actor a) db)

-- Hilfsfunktion ob Genre und Regisseur im Movie sind
is_genre_reg :: Genre -> Regisseur -> Movie -> Bool
is_genre_reg g' r' (_,r,_,_,g,_) = if (g' == g) && (r' == r) then True
                                   else False

-- Hilfsfunktion: Berechnet den neuen Preis
calc_new_price :: Int -> Int -> Int
calc_new_price p d = let x = p + d in
                         if x <= 0 then 1
                         else x

-- Hilfsfunktion: Aktualisiert das Movie-Element wenn Regisseur und Genre uebereinstimmen
update_movie :: Genre -> Regisseur -> Int -> Movie -> Movie
update_movie g' r' v (t,r,m,d,g,s) = if is_genre_reg g' r' (t,r,m,d,g,s) then (t,r,m,d,g,(calc_new_price s v))
                                   else (t,r,m,d,g,s)

upd_dbgri :: Database -> Genre -> Regisseur -> Int -> Database
upd_dbgri db g r v = map (update_movie g r v) db

upd_dbad :: Database -> Actor -> ReleaseDate -> Database
upd_dbad db a d' = filter (\(_,_,m,d,_,_) -> (elem a m) && (d' <= d)) db

get_dbda :: Database -> ReleaseDate -> Actor -> Database
get_dbda db d' a = filter (\(_,_,m,d,_,_) -> (notElem a m) && (d' >= d)) db

sort_dbj :: Database -> Database
sort_dbj db = sortBy (\(_,_,_,d1,_,_) (_,_,_,d2,_,_) -> compare d1 d2) db

-- Aufgabe 2
type ListOfValues = [Integer]
type TargetValue = Integer
type Game = (ListOfValues,TargetValue)
data Operators = Plus | Times | Minus | Div deriving (Eq,Ord,Show)
type Solution = [Operators]

doCalc :: Integer -> Integer -> Operators -> Integer
doCalc m n Plus = m + n
doCalc m n Times = m * n
doCalc m n Minus = m - n
doCalc m n Div = div m n

compResult :: ListOfValues -> Solution -> TargetValue -> Bool
compResult (val:[]) [] target = val == target
compResult (_:0:_) (Div:_) _ = False
compResult (val1:val2:values) (op:slt) target = compResult ([(doCalc val1 val2 op)]++values) slt target

solve' :: ListOfValues -> [Solution] -> TargetValue -> Solution
solve' v [] t = []
solve' v (s:s') t = if (compResult v s t) then s
                    else solve' v s' t

solve :: Game -> Solution
solve ([],_) = []
solve (v,t) = let len = length v in
                  solve' v (replicateM (len-1) [Plus,Minus,Times,Div]) t

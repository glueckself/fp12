data Tree = Null | Tree Label Tree Tree deriving (Eq,Show)
type Label = Integer

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


-- tfold :: (Label -> Label -> Label -> Label) -> Label -> Tree -> Label


type ErdosNumber = Integer
data Scientist = Sc Initial SurName
type Initial = Char
type SurName = String
type Author = Scientist
newtype Database = Db [([Author],PaperTitle)]
type PaperTitle = String


getListOfSc :: Database -> [Scientist] -> [Scientist]
getListOfSc [] _ = []
getListOfSc e:db sc = let result = find (\s -> compare

-- Daten zum Testen
f1 = \x y z -> x+y+z
f2 = \x y z -> x*y*z
t1 = Null
t2 = Tree 2 (Tree 3 Null Null) (Tree 5 Null Null)
t3 = Tree 2 (Tree 3 (Tree 5 Null Null) Null) (Tree 7 Null Null)


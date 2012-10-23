-- Author: Srdjan Markovic (1025857)
import Data.List

-- Bsp. 1
katNumber :: Integer -> Integer

katNumber 0 = error "Argument ungueltig"
katNumber k = let n=k-1 in
              div (product [1..(2*n)]) ((n+1)*(product [1..n])*(product [1..n]))

-- Bsp. 2
sumPowers :: Integer -> Integer -> Integer

sumPowers n k = 
    if k < 0
        then (-1)
       -- else if n < 0
       --     then (0) -- Summe von 1..(n<1)=???
        else sum (map (^k) [1..n])

-- Bsp. 3
-- group erzeugt eine Liste von Listen von Chars, daher ist ein concat
-- notwendig, um die inneren Listen "zusammenzufassen" -> Liste von Chars
-- -> String
shrink :: Char -> String -> String

shrink c s = concat (map (nubBy (\a b -> (a == c)&&(a == b))) (group s))

-- Bsp. 4
-- concat wie bei Bsp 3.
stretch :: Char -> Integer -> String -> String

stretch c n s = let (i::Int) = fromIntegral n in
                concat (map (\e -> if e == c then replicate i c else [e]) s)


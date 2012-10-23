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
        else if n < 0
            then (0)
            else sum (map (^k) [1..n])

-- Bsp. 3
shrink :: Char -> String -> String

shrink c s = concat (map (nubBy (\a b -> (a == c)&&(a == b))) (group s))

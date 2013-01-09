import Data.List

-- Hilfsfunktion, die die Position "unbedingt" zurueckliefert
getPos :: [Integer] -> Integer -> Integer
getPos lst x = let pos = elemIndex x lst in
                   case pos of
                    Just value -> (fromIntegral value)+1
                    Nothing -> 0

-- Hilfsfunktion, die die Zahlenliste nach dem jeweils das n-te Element ignoriert.
-- pos = Anzahl der vorherigen Elemente
luckyNumbers' :: [Integer] -> Integer -> [Integer]
luckyNumbers' (1 : xs) pos = 1 : luckyNumbers' xs (pos+1)
luckyNumbers' (p : xs) pos = p : (luckyNumbers' [x | x <- xs, (mod ((getPos xs x)+pos) p) > 0] (pos+1))

-- 1 soll nicht angezeigt werden, muss aber in der Liste beruecksichtigt werden
luckyNumbers :: [Integer]
luckyNumbers = delete 1 (luckyNumbers' [1,3..] 1)

-- takeWhile, da sonst Endlosschleife
isLuckyNumber :: Integer -> Bool
isLuckyNumber n = elem n (takeWhile (<=n) luckyNumbers)

-- siehe oben
yieldLuckyNumbers :: Integer -> Integer -> [Integer]
yieldLuckyNumbers min max = filter (\e -> (min <= e) ) (takeWhile (<=max) luckyNumbers)

isTwinLuckyNumber :: Integer -> Bool
isTwinLuckyNumber n = (isLuckyNumber n)&&( (isLuckyNumber (n+2))||(isLuckyNumber (n-2)) )

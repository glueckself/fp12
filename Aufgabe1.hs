-- katNumber n = (1/(fromIntegral (n+1)))*((product [1..(2*n)])/(fromIntegral ((product [1..n])*(product [1..n]))))
katNumber :: Integer -> Integer

katNumber 0 = error "Argument ungueltig"
katNumber k = let n=k-1 in
              div (product [1..(2*n)]) ((n+1)*(product [1..n])*(product [1..n]))


{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

import qualified Common as C

main :: IO ()
main = do
  putStr "P003: "
  print $ p003 600851475143

p003 :: Integer -> Integer
p003 = maximum . map snd . C.primeFactors

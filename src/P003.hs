module P003 (main, solveBasic) where

{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

import qualified Common as C

input :: Integer
input = 600851475143

main :: IO ()
main =
  C.time "P003(Basic): " $ solveBasic input

-- 素因数分解してやる
-- 結局これが一番速かった
solveBasic :: Integer -> Integer
solveBasic = maximum . map snd . C.primeFactors

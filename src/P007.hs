module P007 (main, solveBasic) where

{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
that the 6th prime is 13.

What is the 10001st prime number?
-}

import qualified Common as C

input :: Int
input = 10001

main :: IO ()
main = C.time "P007(Basic): " $ solveBasic input

-- 素数列の先頭からN番目を返す
solveBasic :: Int -> Integer
solveBasic = head . flip drop C.primes . flip (-) 1

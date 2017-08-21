{-
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
that the 6th prime is 13.

What is the 10001st prime number?
-}

import qualified Common as C

input :: Int
input = 10001

main :: IO ()
main = -- do
  C.time "P007(Basic): " $ p007Basic input

-- 素数列の先頭からN番目を返す
p007Basic :: Int -> Integer
p007Basic = head . flip drop C.primes . flip (-) 1

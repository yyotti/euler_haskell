module P010 (main, solveBasic) where

{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

import qualified Common as C

input :: Int
input = 2000000

main :: IO ()
main =
  C.time "P010(Basic): " $ solveBasic input

-- 素数列の先頭から足す
-- エラトステネスの篩よりこっちのが速かった（実装が悪かった？）
solveBasic :: Int -> Integer
solveBasic n = sum $ takeWhile (<= fromIntegral n) C.primes -- FIXME Point-Free

module P010 (main, solveBasic, solve, sieve) where

{-
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.
-}

import qualified Common as C

input :: Int
input = 2000000

main :: IO ()
main = -- do
  -- Warning: 時間かかる
  -- C.time "P010(Basic): " $ solveBasic input
  C.time "P010: " $ solve input

-- 素数列の先頭から足す
solveBasic :: Int -> Integer
solveBasic n = sum $ takeWhile (<= fromIntegral n) C.primes -- FIXME Point-Free

sieve :: Int -> [Integer]
sieve n = sieve' [2..fromIntegral n]
  where sieve' [] = []
        sieve' ls@(p:ts) | p*p > fromIntegral n = ls
                         | otherwise = p : sieve' (filter ((/= 0) . (`mod` p)) ts)

-- 最大値が決まっているのでエラトステネスの篩でやる
solve :: Int -> Integer
solve n = sum $ sieve n

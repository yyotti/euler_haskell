module P003 (main, solveBasic, solve) where

{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

import qualified Common as C

input :: Integer
input = 600851475143

main :: IO ()
main = -- do
  -- C.primesが生成されると正確な実行時間が分からなくなるのでBasicはコメント
  -- C.time "P003(Basic): " $ solveBasic input
  C.time "P003: " $ solve input

-- 素因数分解してやる
solveBasic :: Integer -> Integer
solveBasic = maximum . map snd . C.primeFactors

-- 素数で割っていって最大値を得る。
-- 理屈はBasicと同じだが今回に限っては素因数分解が目的ではないのでこちらの方が
-- 速くなる。
solve :: Integer -> Integer
solve n | n < 2 = undefined
        | C.isPrime n = n
        | otherwise = solve' n C.primes
  where solve' _ [] = 0 -- dummy
        solve' k ps@(p:ts) | p*p > k = k
                           | k `mod` p == 0 = solve' (k `div` p) ps
                           | otherwise = solve' k ts

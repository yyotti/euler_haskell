module P006 (main, solveBasic, solve) where

{-
The sum of the squares of the first ten natural numbers is,

    1^2 + 2^2 + ... + 10^2 = 385

The square of the sum of the first ten natural numbers is,

    (1 + 2 + ... + 10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural
numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of the sum.
-}

import qualified Common as C

input :: Int
input = 100

main :: IO ()
main = do
  C.time "P006(Basic): " $ solveBasic input
  C.time "P006: " $ solve input

-- 問題に書いてあるまんま
solveBasic :: Int -> Int
solveBasic n = sum2 - sum1
  where pow2 = flip (^) (2::Int)
        sum1 = sum $ map pow2 [1..n]
        sum2 = pow2 $ sum [1..n]

-- 和の公式を使う
--
-- Nを自然数とする。
-- 1以上N以下の自然数の二乗和は
--   1^2 + 2^2 + ... + N^2 = N(N+1)(2N+1)/6
-- 1以上N以下の自然数の和の二乗は
--   (1 + 2 + ... + N)^2 = {N(N+1)/2}^2
-- これらの差をとればいいので、
--   {N(N+1)/2}^2 - N(N+1)(2N+1)/6 = N(N+1)(N-1)(3N+2)/12
solve :: Int -> Int
solve n = n*(n+1)*(n-1)*(3*n+2) `div` 12

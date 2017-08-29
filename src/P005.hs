module P005 (main, solveBasic, solve) where

{-
2520 is the smallest number that can be divided by each of the numbers from
1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?
-}

import qualified Common as C

input :: Int
input = 20

main :: IO ()
main = do
  C.time "P005(Basic): " $ solveBasic input
  C.time "P005: " $ solve input

-- N以下の素数pについて、p^k <= N を満たす最大のp^kを算出し、全てをかける。
solveBasic :: Int -> Integer
solveBasic n = product $ map (maxPow 1) $ takeWhile (<= fromIntegral n) C.primes
  where maxPow m p | m * p <= fromIntegral n = maxPow (m * p) p
                   | otherwise = m

-- 最小公倍数をとればよい。
solve :: Int -> Integer
solve = foldr lcm 1 . enumFromTo 1 . fromIntegral

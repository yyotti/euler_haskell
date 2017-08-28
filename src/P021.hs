module P021 (main, solveBasic) where

{-
Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n).
If d(a) = b and d(b) = a, where a /= b, then a and b are an amicable pair and
each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71
and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
-}

import qualified Common as C

input :: Int
input = 10000

main :: IO ()
main = -- do
  C.time "P021(Basic): " $ solveBasic input

-- そのままやる
solveBasic :: Int -> Integer
solveBasic n = sum [k | (k, x) <- sums, x > 0, x /= k, sumDivisors x - x == k]
  where sums = [(k, sumDivisors k - k) | k <- [1 .. fromIntegral n]]

sumDivisors :: Integer -> Integer
sumDivisors = product . map (uncurry sumPow) . C.primeFactors
  where sumPow = (sum .) . flip (map . (^)) . enumFromTo 0

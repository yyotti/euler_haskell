module P023 (main, isAbundant, solveBasic) where

{-
A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors of 28
would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than
n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24.
By mathematical analysis, it can be shown that all integers greater than 28123
can be written as the sum of two abundant numbers. However, this upper limit
cannot be reduced any further by analysis even though it is known that the
greatest number that cannot be expressed as the sum of two abundant numbers is
less than this limit.

Find the sum of all the positive integers which cannot be written as the sum
of two abundant numbers.
-}

import qualified Common as C
import qualified Data.Array as A

limit :: Int
limit = 28123

main :: IO ()
main = -- do
  C.time "P023(Basic): " solveBasic

isAbundant :: Int -> Bool
isAbundant n | n < 1 = False
             | otherwise = C.sumDivisors (fromIntegral n) > 2*fromIntegral n

-- そのままやるとめちゃくちゃ遅いので、過剰数判定をメモ化してやる。
-- リストでメモ化するとそれも遅いので、配列を使う。
solveBasic :: Int
solveBasic = sum $ filter (not . isSumOfAbundants) [1..limit]
  where isSumOfAbundants n = any (cache A.!) $ map (n -) $ takeWhile (<= n `div` 2) abundants
        abundants = filter (cache A.!) [1..limit]
        cache = A.listArray (1, limit) $ map isAbundant [1..limit]

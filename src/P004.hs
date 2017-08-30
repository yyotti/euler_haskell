module P004 (main, solveBasic, solve, isPalindrome) where

{-
A palindromic number reads the same both ways. The largest palindrome made
from the product of two 2-digit numbers is 9009 = 91 x 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

import qualified Common as C
import qualified Data.List as L
import qualified Control.Monad.List as M

input :: Int
input = 3

main :: IO ()
main = do
  C.time "P004(Basic): " $ solveBasic input
  C.time "P004: " $ solve input

-- 素直に全パターン探索
solveBasic :: Int -> Int
solveBasic d = maximum $ filter isPalindrome [x*y | x <- [from..to], y <- [x..to]]
  where from = 10^(d-1)
        to = 10^d - 1

isPalindrome :: Int -> Bool
isPalindrome = M.ap (==) rev
  where rev = rev' 0
        rev' i k | k == 0 = i
                 | otherwise = rev' (i * 10 + k `mod` 10) (k `div` 10)

-- 積が回文数になるためには、11の倍数が含まれていなければならない。
--
-- 指定された桁数の11の倍数をx、指定された桁数の数をyとして、x,yを大きい方から
-- かけ合わせていき、各xについて最初に見つかった回文数を列挙したうえで、その
-- 最大値をとればよい。
solve :: Int -> Int
solve d | d == 1 = 9
        | otherwise = maximum $ solve' [to,(to-11)..from]
  where from = 10^(d-1)
        t = 10^d - 1
        to = t - t `mod` 11
        solve' (x:xs) = case L.find isPalindrome (map (x*) [t,(t-1)..from]) of
                             Just n -> n : solve' xs
                             _      -> solve' xs
        solve' _ = [] -- dummy

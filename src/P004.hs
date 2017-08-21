{-
A palindromic number reads the same both ways. The largest palindrome made
from the product of two 2-digit numbers is 9009 = 91 x 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

import qualified Common as C
import qualified Data.List as L

main :: IO ()
main = -- do
  -- C.time "P004(Basic): " $ p004Basic 3
  C.time "P004: " $ p004 3

-- 素直に全パターン探索
{-
p004Basic :: Int -> Int
p004Basic d | d < 1 = 0
            | otherwise = maximum $ filter isPalindrome [x*y | x <- [from..to], y <- [x..to]]
  where from = 10^(d-1)
        to = 10^d - 1
-}

isPalindrome :: Int -> Bool
isPalindrome n | n < 0 = False
               | otherwise = n == rev n 0
  where rev k i | k == 0 = i
                | otherwise = rev (k `div` 10) (i * 10 + k `mod` 10)

-- 積が回文数になるためには、11の倍数が含まれていなければならない。
--
-- 指定された桁数の11の倍数をx、指定された桁数の数をyとして、x,yを大きい方から
-- かけ合わせていき、各xについて最初に見つかった回文数を列挙したうえで、その
-- 最大値をとればよい。
p004 :: Int -> Int
p004 d | d < 1 = 0
       | d == 1 = 9
       | otherwise = maximum $ p004' [to,(to-11)..from]
  where from = 10^(d-1)
        t = 10^d - 1
        to = t - t `mod` 11
        p004' [] = [] -- dummy
        p004' (x:xs) = case L.find isPalindrome (map (x*) [t,(t-1)..from]) of
                            Just n -> n : p004' xs
                            _      -> p004' xs

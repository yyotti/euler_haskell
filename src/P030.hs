module P030 (main, solveBasic) where

{-
Surprisingly there are only three numbers that can be written as the sum of
fourth powers of their digits:

    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4

As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers
of their digits.
-}

import qualified Common as C
import qualified Control.Arrow as A

input :: Int
input = 5

main :: IO ()
main = -- do
  C.time "P030(Basic): " $ solveBasic input

-- 全部調べる
--
-- 1を除くので最小値は2。
-- 最大値の桁数をm桁とすると、
--   10^(m-1) - 1 <= m*9^k  (kは入力された乗数)
-- を見たす最大のmを求めればよい。
solveBasic :: Int -> Int
solveBasic k = sum $ filter (\i -> i == sum (map pow $ C.digits i)) [2..m]
  where m = fst $ head $ dropWhile (uncurry (<=)) $ map ((subtract 1 . (^) 10 . subtract 1) A.&&& (* 9^k)) [1::Int ..]
        pow = (map (^k) [0::Int ..] !!)

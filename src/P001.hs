module P001 (main, solveBasic, solve) where

{-
If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}
import qualified Common as C
import qualified Control.Monad as M

input :: Int
input = 1000

main :: IO ()
main = do
  C.time "P001(Basic): " $ solveBasic input
  C.time "P001: " $ solve input

-- 1から(n-1)で条件に該当する数を順番に足していく
solveBasic :: Int -> Int
solveBasic = sum . filter (M.liftM2 (||) mod3 mod5) . enumFromTo 1 . subtract 1
  where mod3 k = k `mod` 3 == 0
        mod5 k = k `mod` 5 == 0

-- 等差数列の和の公式を用いて
--   3の倍数 + 5の倍数 - 15の倍数
-- を計算する
solve :: Int -> Int
solve n = s 3 + s 5 - s 15
  where k = div $ n - 1
        s d = k d * (d * k d + d) `div` 2

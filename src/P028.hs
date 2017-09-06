module P028 (main, solveBasic) where

{-
Starting with the number 1 and moving to the right in a clockwise direction
a 5 by 5 spiral is formed as follows:

    21 22 23 24 25
    20  7  8  9 10
    19  6  1  2 11
    18  5  4  3 12
    17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
formed in the same way?
-}

import qualified Common as C

input :: Int
input = 1001

main :: IO ()
main = -- do
  C.time "P028(Basic): " $ solveBasic input

-- 前提として、nは奇数でなければならない。
--
-- サイズnの四角形の対角線の和をS(n)とし、サイズnの四角形の各頂点の数字の和を
-- T(n)とすると、
--   S(n) = 1  (n = 1)
--        = T(n) + S(n-2)  (n >= 3)
-- である。
-- また、
--   T(n) = 1  (n = 1)
--        = n^2 + (n^2 - n + 1) + (n^2 - 2n + 2) + (n^2 - 3n + 3)  (n >= 3)
--        = 4n^2 - 6n + 6
-- である。
--
-- n >= 3の場合
-- n >= 3 の場合
-- kを自然数とすれば、n = 2k+1 と書ける。その場合、T(n)をkで表すと
--   T(k) = 4n^2 - 6n + 6
--        = 4(2k+1)^2 - 6(2k+1) + 6
--        = 4(4k^2 + 4k + 1) - 6(2k+1) + 6
--        = 16k^2 + 16k + 4 - 12k - 6 + 6
--        = 16k^2 + 4k + 4
--        = 4(4k^2 + k + 1)
-- S(n) = 1 + Σ(k=1 -> (n-1)/2)T(k)
--      = 1 + 4{4((n-1)/2)(((n-1)/2)+1)(2((n-1)/2)+1)/6 + ((n-1)/2)(((n-1)/2)+1)/2 + ((n-1)/2)}
--      = 1 + 4{4*((n-1)/2)*((n+1)/2)*(n)/6 + ((n-1)/2)*((n+1)/2)/2 + (n-1)/2}
--      = 1 + 4(n-1)*(n+1)*(n)/6 + (n-1)*(n+1)/2 + 2(n-1)
--      = 6/6 + 4(n-1)*(n+1)*(n)/6 + 3(n-1)*(n+1)/6 + 12(n-1)/6
--      = {6 + 4(n-1)*(n+1)*(n) + 3(n-1)*(n+1) + 12(n-1)}/6
--      = {6 + 4n^3-4n + 3n^2-3 + 12n - 12}/6
--      = (4n^3 + 3n^2 + 8n - 9)/6
-- これはS(1)の場合も成立する。
solveBasic :: Int -> Int
solveBasic n = (4*n^(3::Int) + 3*n^(2::Int) + 8*n - 9) `div` 6

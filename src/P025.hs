module P025 (main, solveBasic, solve) where

{-
The Fibonacci sequence is defined by the recurrence relation:

    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.

Hence the first 12 terms will be:

    F1 = 1
    F2 = 1
    F3 = 2
    F4 = 3
    F5 = 5
    F6 = 8
    F7 = 13
    F8 = 21
    F9 = 34
    F10 = 55
    F11 = 89
    F12 = 144

The 12th term, F12, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000
digits?
-}

import qualified Common as C
import qualified Data.List as L

input :: Int
input = 1000

main :: IO ()
main = do
  C.time "P025(Basic): " $ solveBasic input
  C.time "P025: " $ solve input

-- フィボナッチ数列を1つ1つ調べる
solveBasic :: Int -> Int
solveBasic 1 = 1
solveBasic n = case L.findIndex (>= n) (map (length . C.digits) C.fib) of
                    Just x -> x
                    _ -> -1

-- フィボナッチ数列の一般項は
--   F(k) = (P^k)/R5
-- で近似できる(ただし0.5未満で誤差がある)。
-- ここで、Pは黄金比、R5は5の平方根である。
--
-- F(k)の桁数をNとすれば、logを10を底とする対数とした場合、
--   10^(N-1) <= F(k) < 10^N
-- より
--   N-1 <= log10(F(k)) < N
-- が成立する。よって、
--   N-1 <= log10((P^k)/R5) = k*log10(P) - log10(R5)
-- より
--   k*log10(P) >= N-1+log10(R5)
--   k >= (N-1+log10(R5))/log10(P)
-- であるから、これを満たす最初のkを求めればよい。
--
-- NOTE: この計算はN=1の場合は成立しない。そのため、N=1の場合のみ固定値を返す。
solve :: Int -> Int
solve 1 = 1
solve n = ceiling $ (fromIntegral (n - 1) + log10 r5) / log10 phi
  where phi = (1 + r5) / 2 :: Float
        r5 = sqrt 5
        log10 = logBase 10

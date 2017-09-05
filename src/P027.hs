module P027 (main, solveBasic, solve) where

{-
Euler discovered the remarkable quadratic formula:

    n^2 + n + 41

It turns out that the formula will produce 40 primes for the consecutive
integer values 0 <= n <= 39. However, when n = 40, 40^2+40+41 = 40(40+1)+41
is divisible by 41, and certainly when n = 41, 41^2+41+41 is clearly divisible
by 41.

The incredible formula n^2 − 79n + 1601 was discovered, which produces 80
primes for the consecutive values 0 <= n <= 79. The product of the
coefficients, −79 and 1601, is −126479.

Considering quadratics of the form:

    n^2 + an + b, where |a| < 1000 and |b| <= 1000

    where |n| is the modulus/absolute value of n
    e.g. |11|=11 and |−4|=4

Find the product of the coefficients, a and b, for the quadratic expression
that produces the maximum number of primes for consecutive values of n,
starting with n=0.
-}

import qualified Common as C

main :: IO ()
main = do
  C.time "P027(Basic): " solveBasic
  C.time "P027: " solve

maxAB :: Int
maxAB = 1000

-- 全パターンをチェック
solveBasic :: Int
solveBasic = ma * mb
  where f a b n = n*n + a*n + b
        primeLen a b = length $ takeWhile (C.isPrime . fromIntegral) $ map (f a b) [0..]
        (ma, mb) = findMaxAB (0, 0) 0 [(a, b) | a <- [(-(maxAB-1))..(maxAB-1)], a /= 0, b <- [(-maxAB)..maxAB]]
        findMaxAB x _ [] = x
        findMaxAB x m ((a,b):ts) | m < l = findMaxAB (a, b) l ts
                                 | otherwise = findMaxAB x m ts
          where l = primeLen a b

-- f(n) = n^2 + an + b とする。
-- f(0) = b が素数になるので、bは素数でなければならない。
--
-- また、
--   f(n) = n(n+a) + b
-- であるから、nもしくはn+aのどちらかがbの倍数になった場合に素数ではなくなる。
--   1. n > n+a つまり a < 0 の場合
--     n < b の範囲であればbの倍数になることはない
--   2. n <= n+a つまり a >= 0 の場合
--     n+a < b つまり n < b-a の範囲であればbの倍数になることはない
-- 1,2より、a < 0 の方がより多くのnを得られる可能性がある。
--
-- さらに、
--   f(n) = n^2 + an + b
--        = (n + a/2)^2 - (a^2)/4 + b
-- で、これは n = -a/2 の時に最小値 b-(a^2)/4 をもつが、 f(n) <= 0 の範囲は
-- 調べる意味がないので
--   b - (a^2)/4 > 0
-- の範囲で調べればよい。つまり
--   b > (a^2)/4
--   4b > a^2
-- a < 0 なので
--   -2*sqrt(b) < a < 0
-- である。
--
-- bを大きい方から調べていき、得られたnの最大値をmとする。
-- nは最大でもb-1までなので、b <= m となったらそこで探索を打ち切ってよい。
solve :: Int
solve = ma * mb
  where ab = [(fromInteger a, fromInteger b) | b <- reverse $ takeWhile (<= fromIntegral maxAB) C.primes,
                                               a <- [((-2) * isqrt b)..(-1)]]
        f a b n = n*n + a*n + b
        primeLen a b = length $ takeWhile (C.isPrime . fromIntegral) $ map (f a b) [0..]
        (ma, mb) = findMaxAB (0, 0) 0 ab
        findMaxAB x _ [] = x
        findMaxAB x m ((a,b):ts) | m < l = findMaxAB (a, b) l ts
                                 | otherwise = findMaxAB x m ts
          where l = primeLen a b

isqrt :: Integral a => a -> a
isqrt = floor . (sqrt :: Float -> Float) . fromIntegral

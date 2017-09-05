module P026 (main, solveBasic, rec, solve) where

{-
A unit fraction contains 1 in the numerator. The decimal representation of the
unit fractions with denominators 2 to 10 are given:

    1/2  = 0.5
    1/3  = 0.(3)
    1/4  = 0.25
    1/5  = 0.2
    1/6  = 0.1(6)
    1/7  = 0.(142857)
    1/8  = 0.125
    1/9  = 0.(1)
    1/10 = 0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle
in its decimal fraction part.
-}

import qualified Common as C
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Ord as O

input :: Int
input = 1000

main :: IO ()
main = do
  C.time "P026(Basic): " $ solveBasic input
  C.time "P026: " $ solve input

-- 割り算の筆算のやり方でやる
solveBasic :: Int -> Int
solveBasic = L.maximumBy ((. recLen) . O.compare . recLen) . enumFromTo 1
  where recLen = (map (length . rec) [0..] !!)

rec :: Int -> [Int]
rec n = rec' n 1 0 M.empty []
  where rec' :: Int -> Int -> Int -> M.Map Int Int -> [Int] -> [Int]
        rec' k r i m ls | r == 0 = []
                        | M.member r m = drop (m M.! r) $ reverse ls
                        | otherwise = rec' k (r * 10 `mod` k) (i+1) (M.insert r i m) ((r * 10 `div` k) : ls)

-- https://ja.wikipedia.org/wiki/%E5%BE%AA%E7%92%B0%E5%B0%8F%E6%95%B0
--
-- 有理数の分母の素因数が基数の約数たる素数のみである場合、有限小数表示になる。
-- 今回は10進数を対象としているので、基数の約数たる素数は2と5である。よって、
-- 分母が素因数に2と5以外をもたない場合は循環節の長さは最大にはなりえない。
--
-- また、有理数を整数倍しても循環節の長さに変化はないので、分母が素因数に2もし
-- くは5をもつ場合、それらの因子を排除して残った数で循環節の長さを求めればよい。
--
-- 以下、分母は素因数に2と5をもたないものとする。
--
-- 循環節の長さは、筆算で小数表示と各段階での余りを出す操作を繰り返し、過去に
-- 出現した余りと同じ値が再度出現した時点までの長さをとる。nで割った余りは、
-- 割り切れて0になる場合を除くと 1以上(n-1)以下 の(n-1)通りであるから、循環節
-- の長さは最大でも(n-1)である。
--
-- カーマイケル関数をl(n)とすると、nが奇素数pを用いて n = p^e と書けるなら
--   l(n) = (p-1)*p^(e-1)
-- である。
-- また、nの素因数分解を p1^e1 * p2^e2 ... と書く場合、
--   l(n) = lcm(l(p1^e1), l(p2^e2), ...)
-- である。よって、下記の3パターンに分けて考える。
--   1. nが素数である場合
--   2. nが p^e (e>=2)である場合
--   3. nが p1^e1 * p2^e2 ... と素因数分解できる場合
--   (ただし、1～3で使用する素数は全て2でも5でもないとする)
--
-- 1. nが素数である場合
--   l(n) = (p-1)*p^(1-1) = p-1 = n-1
-- 2. nがp^e(e>=2)である場合
--     l(n) = (p-1)*p^(e-1)
--   であるが、これと (n-1) を比較すると
--     (n-1) - (p-1)*p^(e-1) = (p^e - 1) - (p*p^(e-1) - p^(e-1))
--                           = p^e - 1 - p^e + p^(e-1)
--                           = p^(e-1) - 1 > 0
--   であるから、
--     n-1 > (p-1)*p^(e-1)
--   となる。
--   nは素因数に2と5をもたないので、10とは互いに素な自然数であり、その場合の
--   循環節の長さはたかだか l(n) であるため、1の結果からnが素数である方が
--   循環節が長くなる可能性がある。
-- 3. nが p1^e1 * p2^e2 ... と素因数分解できる場合
--     l(n) = lcm(l(p1^e1), l(p2^e2), ...)
--   であり、 p1,p2,... に2は含まれず、かつ e1,e2,... >= 1 であるから、
--     l(n) = lcm((p1 - 1)*p1^(e1 - 1), (p2 - 1)*p2^(e2 - 1), ...)
--          = (p1 - 1)*p1^(e1 - 1)*(p2 - 1)*p2^(e2 - 1)*...*pm^(em - 1) / k
--   となる。p1,p2,...は全て互いに素であるから、
--     l(n) = lcm((p1 - 1)*p1^(e1 - 1), (p2 - 1)*p2^(e2 - 1), ...)
--          = (p1 - 1)*p1^(e1 - 1) * (p2 - 1)*p2^(e2 - 1) * ...
--   となる。これと (n-1) を比較すると
--     (n-1) - (p1 - 1)*p1^(e1 - 1) * (p2 - 1)*p2^(e2 - 1) * ...
--       = p1^e1 * p2^e2 * ... - 1 - {((p1-1)/p1)*p1^e1 * ((p2-1)/p2)*p2^e2 * ...}
--       = p1^e1 * p2^e2 * ... - 1 - {(1-1/p1)*(1-1/p2)*... * p1^e1*p2^e2*...}
--       = n - 1 - {(1-1/p1)*(1-1/p2)*... * n}
--       = n - 1 - {(1-1/p1)*(1-1/p2)*... * n}
--       > 0
--   (1/p1,1/p2,...は全て1より小さくなるため、{(1-1/p1)*(1-1/p2)*... * n}は1よ
--   り小さくなり、n >= 15 であるから)
--
-- 上記の1～3より、循環節の長さが最長となる可能性があるのはnが素数の場合である。
-- ただし、その長さは n - 1 の約数であるため、nが大きい方から順に素数を調べて
-- いく必要がある。
-- 素数nの逆数の循環節の長さは、循環節をm桁とすると
--   10^m - 1
-- が n-1 の約数で割り切れるので、小さい方のmから順に調べていき最初に見つけた
-- m とすればよい。
solve :: Int -> Int
solve n | n < 3 = 0
        | otherwise = fromInteger $ findMaxP ps 0
  where ps = reverse $ takeWhile (< fromIntegral n) $ drop 1 C.primes
        findMaxP [] m = m
        findMaxP (h:ts) m | lp == h - 1 = h
                          | m < lp = findMaxP ts lp
                          | otherwise = findMaxP ts m
          where lp = case L.find ((== 0) . (`mod` h) . subtract 1 . (10^)) (C.divisors (h-1)) of
                          Just x -> x
                          _ -> undefined

{-
A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

    a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
-}

import qualified Common as C

input :: Int
input = 1000

main :: IO ()
main = -- do
  -- C.time "P009(Basic): " $ p009Basic input
  C.time "P009: " $ p009 input

-- 全パターンを試す。
-- ただし、足してNになるパターンが1つだけというのは信用しないことにする。
--
-- a + b + c = N とすれば c = N - a - b であるから、a < b を満たすようにリスト
-- を作ればよい。しかし c >= b になる可能性もあるのでフィルターする。
{-
p009Basic :: Int -> Int
p009Basic s = case prods of
                   [] -> 0
                   _ -> maximum prods
  where prods = [a*b*c | a <- [1..s], b <- [(a+1)..s], c <- [s - a - b], b < c, a*a + b*b == c*c, a + b + c == s]
-}

-- 原始ピタゴラス数を列挙して考える
p009 :: Int -> Int
p009 s = case prods of
                   [] -> 0
                   _ -> maximum prods
  where pytha = [(m*m - n*n, 2*m*n, m*m + n*n) | m <- [2,3..maxM], n <- [1..(m-1)],
                                                 (m - n) `mod` 2 == 1, gcd m n == 1,
                                                 s `mod` (2*m*m + 2*m*n) == 0]
        prods = [a*b*c*d*d*d | (a, b, c) <- pytha, d <- [s `div` (a + b + c)]]
        maxM = floor $ sqrt $ fromIntegral s  -- FIXME HLint warning

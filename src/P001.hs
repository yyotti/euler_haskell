{-
If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}
import qualified Common as C

input :: Int
input = 1000

main :: IO ()
main = -- do
  -- C.time "P001(Basic): " $ p001Basic input
  C.time "P001: " $ p001 input

{-
p001Basic :: Int -> Int
p001Basic n = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..(n-1)]
-}

-- 等差数列の和の公式を用いて
--   3の倍数 + 5の倍数 - 15の倍数
-- を計算する
p001 :: Int -> Int
p001 n = s 3 + s 5 - s 15
  where k = div $ n - 1
        s d = k d * (d * k d + d) `div` 2

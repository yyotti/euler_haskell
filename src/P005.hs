{-
2520 is the smallest number that can be divided by each of the numbers from
1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?
-}

import qualified Common as C

main :: IO ()
main = -- do
  -- C.time "P005(Basic): " $ p005Basic 20
  C.time "P005: " $ p005 20

-- N以下の素数pについて、p^k <= N を満たす最大のp^kを算出し、全てをかける。
{-
p005Basic :: Int -> Integer
p005Basic n = product $ map (maxPow 1) $ takeWhile (<= fromIntegral n) C.primes
  where maxPow m p | m * p <= fromIntegral n = maxPow (m * p) p
                   | otherwise = m
-}

-- 最小公倍数をとればよい。
p005 :: Int -> Integer
p005 n = foldr lcm (1::Integer) [1..(fromIntegral n)]

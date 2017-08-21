{-
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?
-}

import qualified Common as C

main :: IO ()
main = -- do
  -- C.time "P003(Basic): " $ p003Basic 600851475143
  C.time "P003: " $ p003 600851475143

-- 素因数分解してやる
{-
p003Basic :: Integer -> Integer
p003Basic = maximum . map snd . C.primeFactors
-}

-- 素数で割っていって最大値を得る。
-- 理屈はBasicと同じだが今回に限っては素因数分解が目的ではないのでこちらの方が
-- 速くなる。
p003 :: Integer -> Integer
p003 n | C.isPrime n = n
            | otherwise = p003' n C.primes
  where p003' _ [] = 0 -- dummy
        p003' k ps@(p:ts) | p*p >= k = k
                          | k `mod` p == 0 = p003' (k `div` p) ps
                          | otherwise = p003' k ts

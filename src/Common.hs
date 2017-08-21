module Common (
  fib,
  primes,
  isPrime,
  primeFactors
) where
import qualified Data.List as L
import qualified Control.Arrow as A

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)

primes :: [Integer]
primes = 2 : filter isPrime [3, 5 ..]

isPrime :: Integer -> Bool
isPrime n | n < 2 = False
          | otherwise = all (\x -> n `mod` x /= 0) $ takeWhile (\x -> x*x <= n) primes

primeFactors :: Integer -> [(Int, Integer)]
primeFactors n | n < 1 = []
               | otherwise = pack $ pf n primes
  where pf _ [] = []
        pf 1 _ = []
        pf k ps@(p:ts) | k `mod` p == 0 = p : pf (k `div` p) ps
                       | otherwise = pf k ts
        pack = map (length A.&&& head) . L.group

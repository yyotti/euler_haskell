module Common (
  time,
  fib,
  primes,
  isPrime,
  primeFactors
) where
import qualified Data.List as L
import qualified Control.Arrow as A
import qualified Data.Time as T

time :: Show a => String -> a -> IO ()
time pre f = do
  s <- T.getCurrentTime
  putStr $ pre ++ show f
  e <- T.getCurrentTime
  putStrLn $ " (" ++ show (T.diffUTCTime e s) ++ ")"

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)

-- primes :: [Integer]
-- primes = 2 : filter isPrime [3, 5 ..] -- My original

primes :: [Integer]
primes = 2 : 3 : 5 # primes
  where _ # [] = [] -- dummy
        _ # [_] = [] -- dummy
        n # x@(m:p:ys) = [n | gcd m n < 2] ++ (n+2) # last (x : [m*p : ys | p*p - 3 < n])

-- FIXME Miller-Rabin
isPrime :: Integer -> Bool
isPrime n | n < 2 = False
          | otherwise = all ((/= 0) . (n `mod`)) . takeWhile ((<= n) . (^(2::Integer))) $ primes

primeFactors :: Integer -> [(Int, Integer)]
primeFactors n | n < 1 = []
               | otherwise = pack $ pf n primes
  where pf _ [] = []
        pf 1 _ = []
        pf k ps@(p:ts) | p*p > k = [k]
                       | k `mod` p == 0 = p : pf (k `div` p) ps
                       | otherwise = pf k ts
        pack = map (length A.&&& head) . L.group

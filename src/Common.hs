module Common (
  time,
  fib,
  primes,
  isPrime,
  primeFactors,
  digits,
  permutation,
  fact,
  combination,
  sumDivisors
) where
import qualified Data.List as L
import qualified Control.Arrow as A
import qualified Data.Time as T
import qualified Data.Char as C

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
  where n # x@(m:p:ys) = [n | gcd m n < 2] ++ (n+2) # last (x : [m*p : ys | p*p - 3 < n])
        _ # _ = [] -- dummy

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

digits :: (Integral a, Show a) => a -> [Int]
digits n | n < 0 = []
         | otherwise = map C.digitToInt $ show n

permutation :: Int -> Int -> Integer
permutation n r | n < 0 || r < 0 || n < r = 0
                | n == 0 || r == 0 = 1
                | otherwise = fromIntegral n * permutation (n-1) (r-1)

fact :: Int -> Integer
fact n = permutation n n

combination :: Int -> Int -> Integer
combination n r | n < 0 || r < 0 = 0
                | otherwise = permutation n r `div` fact r

sumDivisors :: Integer -> Integer
sumDivisors = product . map (uncurry sumPow) . primeFactors
  where sumPow = (sum .) . flip (map . (^)) . enumFromTo 0

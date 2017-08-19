module Common (
  fib
) where
fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)

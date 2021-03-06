module P002 (main, solveBasic, solve) where

{-
Each new term in the Fibonacci sequence is generated by adding the previous
two terms. By starting with 1 and 2, the first 10 terms will be:

    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed
four million, find the sum of the even-valued terms.
-}

import qualified Common as C

input :: Int
input = 4000000

main :: IO ()
main = do
  C.time "P002(Basic): " $ solveBasic input
  C.time "P002: " $ solve input

-- ベーシックというか、フィボナッチ数列本来の再帰定義を用いる
--
-- せめてメモ化再帰にしてみる
solveBasic :: Int -> Int
solveBasic = sum . filter even . flip takeWhile (map fibonacci [2..]) . flip (<=)
  where fibonacci = (map f [0..] !!)
        f 0 = 0
        f 1 = 1
        f k = fibonacci (k-2) + fibonacci (k-1)

solve :: Int -> Int
solve = sum . filter even . flip takeWhile fib12 . flip (<=)
  where fib12 = map fromInteger $ drop 2 C.fib

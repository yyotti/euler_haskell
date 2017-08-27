module P020 (main, solveBasic) where

{-
n! means n x (n − 1) x ... x 3 x 2 x 1

For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800,
and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
-}

import qualified Common as C

input :: Int
input = 100

main :: IO ()
main = -- do
  C.time "P020(Basic): " $ solveBasic input

-- そのままやる
solveBasic :: Int -> Int
solveBasic n = sum $ C.digits $ C.fact n

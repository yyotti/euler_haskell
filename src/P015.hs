module P015 (main, solveBasic) where

{-
Starting in the top left corner of a 2x2 grid, and only being able to move to
the right and down, there are exactly 6 routes to the bottom right corner.

    (images)

How many such routes are there through a 20x20 grid?
-}

import qualified Common as C

input :: Int
input = 20

main :: IO ()
main = -- do
  C.time "P015(Basic): " $ solveBasic input

-- 入力をNとすると、(2N)C(N)を計算すればよい
solveBasic :: Int -> Integer
solveBasic n = combination (2*n) n

permutation :: Int -> Int -> Integer
permutation _ 0 = 1
permutation n r = fromIntegral n * permutation (n-1) (r-1)

combination :: Int -> Int -> Integer
combination n r = permutation n r `div` permutation r r

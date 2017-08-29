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
solveBasic = C.combination =<< (2*)

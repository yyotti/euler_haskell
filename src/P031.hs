module P031 (main, solveBasic) where

{-
In England the currency is made up of pound, £, and pence, p, and there are
eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1x£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p

How many different ways can £2 be made using any number of coins?
-}

import qualified Common as C
import qualified Data.List as L

inputA :: Int
inputA = 200

inputB :: [Int]
inputB = [1, 2, 5, 10, 20, 50, 100, 200]

main :: IO ()
main = -- do
  C.time "P031(Basic): " $ solveBasic inputA inputB

-- 特に工夫はない
solveBasic :: Int -> [Int] -> Int
solveBasic 0 _ = 1
solveBasic _ [] = 0
solveBasic p cs = sum $ map (flip solveBasic ts . flip subtract p . (h *)) [0..m]
  where (h:ts) = L.sortBy (flip compare) cs
        m = p `div` h

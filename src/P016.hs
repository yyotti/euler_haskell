module P016 (main, solveBasic) where

{-
2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
-}

import qualified Common as C

input :: Int
input = 1000

main :: IO ()
main = -- do
  C.time "P015(Basic): " $ solveBasic input

-- 単純に2^1000を出して桁を足す
solveBasic :: Int -> Int
solveBasic = sum . C.digits . ((2::Integer)^)

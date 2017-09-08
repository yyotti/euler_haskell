module P032 (main, solveBasic) where

{-
We shall say that an n-digit number is pandigital if it makes use of all the
digits 1 to n exactly once; for example, the 5-digit number, 15234,
is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity
can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only
include it once in your sum.
-}

import qualified Common as C
import qualified Data.List as L
import qualified Data.Set as S

input :: Int
input = 9

main :: IO ()
main = -- do
  C.time "P032(Basic): " $ solveBasic input

-- 1からNまでの数で順列を作り、それぞれで掛け算が成立するかをチェックする。
-- なんかくちゃくちゃになった
solveBasic :: Int -> Integer
solveBasic = sum . foldr (S.insert . C.toNum . (!! 2)) S.empty . filter (\[a, b, c] -> C.toNum a * C.toNum b == C.toNum c) . concatMap triplet . L.permutations . enumFromTo 1
  where triplet ps = concatMap ((\[a, n] -> map (\i -> [a, take i n, drop i n]) [length a .. length n `div` 2]) . (\i -> [take i ps, drop i ps])) [1 .. length ps `div` 3]

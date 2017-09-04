module P024 (main, solveBasic, permutations, solve1, solve2, factoradic, nthPermutation) where

{-
A permutation is an ordered arrangement of objects. For example, 3124 is one
possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
are listed numerically or alphabetically, we call it lexicographic order.
The lexicographic permutations of 0, 1 and 2 are:

    012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
5, 6, 7, 8 and 9?
-}

import qualified Common as C
import qualified Data.Char as Ch
import qualified Data.List as L

main :: IO ()
main = do
  C.time "P023(Basic): " $ solveBasic [0..9] 1000000
  C.time "P023 - 1: " $ solve1 [0..9] 1000000
  C.time "P023 - 2: " $ solve2 [0..9] 1000000

toNum :: [Int] -> Integer
toNum ls = read $ map Ch.intToDigit ls

-- 順列を作って指定された番号の要素をとる
-- Data.List.permutations だと並びが違うので自前で作る
solveBasic :: [Int] -> Int -> Integer
solveBasic ls n = toNum $ permutations ls !! (n-1)

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations [x] = [[x]]
permutations ls = concatMap (\x -> map (x:) $ permutations (filter (/= x) ls)) ls

-- リスト[d1,d2,...,dn]が与えられ、順列のN番目をとるものとする。
--
-- 先頭に置く数字をa1とする。a1の候補は与えられたリストの全要素である。
-- a1を決めたらその後ろに続く数字の順列は (n-1)! 通りあるので、a1=dkとするなら
-- k*(n-1)! < N を満たさなければならない。
--
-- kを見つけたら、
--   リスト = [d1,d2,...,d(k-1),d(k+1),...,dn]
--   N = N - k*(n-1)!
-- として、再帰的に次の桁を確定する。
solve1 :: [Int] -> Int -> Integer
solve1 ds = toNum . solve1' ds
  where solve1' [] _ = []
        solve1' ls@(_:ts) n = (ls !! k) : solve1' (dropN k ls) (n - k*f)
          where k = subtract 1 $ length $ takeWhile (< n) $ map (f*) [0..]
                f = fact $ length ts

dropN :: Int -> [a] -> [a]
dropN _ [] = []
dropN 0 (_:ts) = ts
dropN n (h:ts) = h : dropN (n-1) ts

-- FIXME Common.factの型をIntegralにする
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

-- 階乗進数でやる
solve2 :: [Int] -> Int -> Integer
solve2 = (toNum .) . nthPermutation

factoradic :: Int -> [Int]
factoradic = reverse . factoradic' 2
  where factoradic' _ 0 = []
        factoradic' k d = d `mod` k : factoradic' (k+1) (d `div` k)

nthPermutation :: [a] -> Int -> [a]
nthPermutation [] _ = []
nthPermutation ls n = reverse $ fst $ L.foldl' (\(p, l) k -> (l !! k : p, dropN k l)) ([], ls) fs
  where fs = take (length ls - length fs') [0,0..] ++ fs'
        fs' = factoradic (n - 1) ++ [0]

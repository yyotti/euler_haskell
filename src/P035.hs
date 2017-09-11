module P035 (main, rotate, solveBasic) where

{-
The number, 197, is called a circular prime because all rotations of the
digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
73, 79, and 97.

How many circular primes are there below one million?
-}

import qualified Common as C
import qualified Data.Map as M
import qualified Data.List as L

input :: Int
input = 1000000

main :: IO ()
main = -- do
  C.time "P035(Basic): " $ solveBasic input

rotate :: Int -> [Int]
rotate n = map (fromInteger . C.toNum) $ rotate' ds $ length ds
  where ds = C.digits n
        rotate' _ 0 = []
        rotate' ns@(h:ts) r =  ns : rotate' (ts ++ [h]) (r-1)
        rotate' _ _ = undefined

-- 指定範囲の素数を全部調べる
-- ある素数が循環素数か否かが分かればそれを循環させた数も同じ結果になるので、
-- キャッシュしながらやれば多重判定しなくてよくなる。
-- また、2桁以上の素数に関しては、どこかの桁に偶数もしくは5が入っていると循環
-- した際に素数にならない数が出るので、全ての桁が奇数で構成されていなければな
-- らない。
solveBasic :: Int -> Int
solveBasic m = snd $ L.foldl' (uncurry f) (M.empty, 0) ps
  where ps = map fromInteger $ filter (\p -> p < 10 || isAllOdd p) $ takeWhile (< fromIntegral m) C.primes
        f cache cnt p | M.member p cache = (cache, cnt + if cache M.! p then 1 else 0)
                      | isCircularPrime p = (addCache p True cache, cnt + 1)
                      | otherwise = (addCache p False cache, cnt)
        isCircularPrime = all C.isPrime . map fromIntegral . rotate
        addCache = flip (flip . foldr . flip M.insert) . rotate
        isAllOdd = all odd . C.digits

module P034 (main, solveBasic, solve) where

{-
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of
their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
-}

import qualified Common as C
import qualified Control.Monad as M
import qualified Data.List as L

main :: IO ()
main = do
  C.time "P034(Basic): " solveBasic
  C.time "P034: " solve

-- 1!と2!は除くので、1桁の数に該当するものはない。
--
-- nの桁数をmとし、
--   n = a[m-1]*10^(m-1) + ... + a2*10^2 + a1*10^1 + a0
-- とすると、
--   s = a[m-1]! + a[m-2]! + ... + a1! + a0! < n
-- となる最大のmを考える。
-- a[k]の最大値は9!であり、m桁の数の最小値は10^(m-1)であるから、
--   m*9! < 10^(m-1)
-- を見たすmを見つければよい。
--   m = 7のとき 7*9! = 2540160 > 10^6
--   m = 8のとき 8*9! = 2903040 < 10^7
-- であるから、7桁の数まで調べればよいことになる。
--
-- 階乗の計算は何度も行うので、0～9の各階乗はキャッシュしておく。
solveBasic :: Int
solveBasic = sum $ map fromInteger $ filter isFactSum [10 .. 9999999]
  where f = (map C.fact [0..] !!)
        isFactSum = M.ap (==) $ (sum . map f) . C.digits

-- http://d.hatena.ne.jp/D_Rascal/20120118/1326890749
-- 桁の並びが変わっても階乗和は変化しないので、例えば145を調べて条件を満たすな
-- らば、415や541は調べなくてよい。
solve :: Integer
solve = sum $ map f $ concat $ filter isFactSum nums
  where nums = concatMap (select [0..9]) [2..7]
        f = (map C.fact [0..] !!)
        isFactSum ls = sumDigits == ls
          where sumDigits = L.sort $ map (read . (:"")) $ show $ sum $ map f ls
        select _ 0 = [[]]
        select [x] n = [replicate n x]
        select (h:ts) n = concatMap step [0..n]
          where step t = map (replicate t h ++) $ select ts $ n - t
        select _ _ = undefined

module P033 (main, solveBasic, solve) where

{-
The fraction 49/98 is a curious fraction, as an inexperienced mathematician
in attempting to simplify it may incorrectly believe that 49/98 = 4/8,
which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less
than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms,
find the value of the denominator.
-}

import qualified Common as C
import Data.Ratio

main :: IO ()
main = do
  C.time "P033(Basic): " solveBasic
  C.time "P033: " solve

-- 言われている通りに列挙する
--
-- 1より小さい分数で、分母分子に2桁の数字があるものなので、r/dとすると
--   11 <= d <= 99
--   10 <= r < d
-- である。
solveBasic :: Int
solveBasic = denominator $ foldr ((*) . uncurry (%)) 1 $ filter isCancellingFraction fracs
  where fracs = [(r, d) | d <- [11..99], r <- [10..(d-1)]]
        isCancellingFraction (r, d) | r1 == 0 || d1 == 0 = False
                                    | r0 == d0 = frac == r1 % d1
                                    | r0 == d1 = frac == r1 % d0
                                    | r1 == d0 = frac == r0 % d1
                                    | r1 == d1 = frac == r0 % d0
                                    | otherwise = False
                                        where frac = r % d
                                              (r0, r1) = r `divMod` 10
                                              (d0, d1) = d `divMod` 10

-- 分母分子ともに2桁の分数から、分母分子とも1桁ずつ削除するので、1桁/1桁の分数
-- になる。よって、逆に 1桁/1桁 の分数 r/d (r < d) に対して、分母分子に0ではな
-- い同じ数字を付与して値が同じになるものを探す。
solve :: Int
solve = denominator $ foldr ((*) . uncurry (%)) 1 $ filter isCancellingFraction fracs
  where fracs = [(r, d) | d <- [2..9], r <- [1..(d-1)]]
        isCancellingFraction (r, d) = elem (r % d) $ concatMap (\a -> [(r*10 + a) % (d*10 + a),
                                                                       (r*10 + a) % (a*10 + d),
                                                                       (a*10 + r) % (d*10 + a),
                                                                       (a*10 + r) % (a*10 + d)]) [1..9]

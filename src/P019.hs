module P019 (main, solveBasic) where

{-
You are given the following information, but you may prefer to do some
research for yourself.

・1 Jan 1900 was a Monday.
・Thirty days has September, April, June and November.  All the rest have
  thirty-one, Saving February alone, Which has twenty-eight, rain or shine.
  And on leap years, twenty-nine.
・A leap year occurs on any year evenly divisible by 4, but not on a century
  unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century
(1 Jan 1901 to 31 Dec 2000)?
-}

import qualified Common as C

main :: IO ()
main = -- do
  C.time "P019(Basic): " solveBasic

-- 与えられた情報をもとに地道にカウントする
solveBasic :: Int
solveBasic = length $ filter id $ take (12 * 100) $ map isSunday $ drop 12 dates
  where isLeapYear n = n `mod` 400 == 0 || n `mod` 4 == 0 && n `mod` 100 /= 0
        dateCount y m | m `elem` [4,6,9,11] = 30
                      | m == 2 = if isLeapYear y then 29 else 28
                      | otherwise = 31
        dates = scanl (+) 1 $ map (uncurry dateCount) [(y,m) | y <- [1900::Int ..], m <- [1::Int .. 12]]
        isSunday = (== (0::Int)) . (`mod` 7)

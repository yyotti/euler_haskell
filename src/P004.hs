{-
A palindromic number reads the same both ways. The largest palindrome made
from the product of two 2-digit numbers is 9009 = 91 x 99.

Find the largest palindrome made from the product of two 3-digit numbers.
-}

import qualified Data.List as L

main :: IO ()
main = do
  putStr "P004: "
  print $ p004 3

p004 :: Int -> Int
p004 d | d < 1 = 0
       | d == 1 = 9
       | otherwise = maximum $ p004' [to,(to-11)..from]
  where from = 10^(d-1)
        t = 10^d - 1
        to = t - t `mod` 11
        isPalindrome n | n < 0 = False
                       | otherwise = n == rev n 0
        rev k i | k == 0 = i
                | otherwise = rev (k `div` 10) (i * 10 + k `mod` 10)
        p004' [] = []
        p004' (x:xs) = case L.find isPalindrome (map (x*) [t,(t-1)..x]) of
                            Just n -> n : p004' xs
                            _      -> p004' xs

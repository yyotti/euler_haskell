{-
If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

main :: IO ()
main = do
  putStr "P001: "
  print $ p001 1000

p001 :: Int -> Int
p001 n = s 3 + s 5 - s 15
  where k = div $ n - 1
        s d = k d * (d * k d + d) `div` 2

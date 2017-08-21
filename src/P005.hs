{-
2520 is the smallest number that can be divided by each of the numbers from
1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the
numbers from 1 to 20?
-}

main :: IO ()
main = do
  putStr "P005: "
  print $ p005 20

p005 :: Integer -> Integer
p005 n = foldr lcm (1::Integer) [1..n]

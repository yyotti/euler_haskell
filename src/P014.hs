module P014 (main, solveBasic, collatz, collatzChainLen, collatzChainLenMem, solve) where

{-
The following iterative sequence is defined for the set of positive integers:

    n -> n/2 (n is even)
    n -> 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

    13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains
10 terms. Although it has not been proved yet (Collatz Problem), it is thought
that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
-}

import qualified Common as C
import qualified Control.Arrow as A

input :: Int
input = 1000000

main :: IO ()
main = do
  C.time "P014(Basic): " $ solveBasic input
  C.time "P014: " $ solve input

collatz :: Int -> Int
collatz n | n `mod` 2 == 0 = n `div` 2
          | otherwise = 3*n + 1

collatzChainLen :: Int -> Int
collatzChainLen 1 = 0
collatzChainLen n = 1 + collatzChainLen (collatz n)

-- 全ての数でコラッツ数列を出してその長さをとる
-- コラッツ数列そのものが欲しいわけではないので、長さだけカウントする
solveBasic :: Int -> Int
solveBasic = snd . maximum . map (collatzChainLen A.&&& id) . flip take [1..]

-- メモ化を使う
-- http://d.hatena.ne.jp/tanakh/20100411

data Tree a = Tree a (Tree a) (Tree a)

findTree :: Tree b -> Int -> b
findTree tree ix = f (bits $ ix + 1) tree
  where f [] (Tree v _ _) = v
        f (0:bs) (Tree _ l _) = f bs l
        f (_:bs) (Tree _ _ r) = f bs r
        bits = tail . reverse . map (`mod` 2) . takeWhile (> 0) . iterate (`div` 2)

genTree :: (Int -> b) -> Tree b
genTree f = gen 0
  where gen ix = Tree (f ix) (gen $ ix*2 + 1) (gen $ ix*2 + 2)

memofix :: ((Int -> b) -> (Int -> b)) -> (Int -> b)
memofix f = memof
  where memof = f $ findTree tbl
        tbl = genTree memof

collatzChainLenMem :: Int -> Int
collatzChainLenMem = memofix $ \f n -> if n == 1 then 0 else 1 + f (collatz n)

solve :: Int -> Int
solve = snd . maximum . map (collatzChainLenMem A.&&& id) . flip take [1..]

{- FIXME 他の実装を試してみる -}

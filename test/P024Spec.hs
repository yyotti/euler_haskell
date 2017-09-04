module P024Spec where

import qualified P024 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solveBasic" $ do
    it "与えられた数値でできるN番目の順列 - 1" $
      P.solveBasic [0] 1 `shouldBe` 0
    it "与えられた数値でできるN番目の順列 - 2" $
      map (P.solveBasic [1, 2]) [1..2] `shouldBe` [12, 21]
    it "与えられた数値でできるN番目の順列 - 3" $
      map (P.solveBasic [0, 1, 2]) [1..6] `shouldBe` [12, 21, 102, 120, 201, 210]

  describe "permutations" $ do
    it "空リストなら空リストのみ" $
      P.permutations ([]::[Int]) `shouldBe` ([[]]::[[Int]])
    it "順列 - 1" $
      P.permutations [1::Int] `shouldBe` [[1]]
    it "順列 - 2" $
      P.permutations "ab" `shouldBe` ["ab", "ba"]
    it "順列 - 3" $
      P.permutations [0::Int, 1, 2] `shouldBe` [[0,1,2], [0,2,1], [1,0,2], [1,2,0], [2,0,1], [2,1,0]]

  describe "solve1" $ do
    it "与えられた数値でできるN番目の順列 - 1" $
      P.solve1 [0] 1 `shouldBe` 0
    it "与えられた数値でできるN番目の順列 - 2" $
      map (P.solve1 [1, 2]) [1..2] `shouldBe` [12, 21]
    it "与えられた数値でできるN番目の順列 - 3" $
      map (P.solve1 [0, 1, 2]) [1..6] `shouldBe` [12, 21, 102, 120, 201, 210]

  describe "solve2" $ do
    it "与えられた数値でできるN番目の順列 - 1" $
      P.solve2 [0] 1 `shouldBe` 0
    it "与えられた数値でできるN番目の順列 - 2" $
      map (P.solve2 [1, 2]) [1..2] `shouldBe` [12, 21]
    it "与えられた数値でできるN番目の順列 - 3" $
      map (P.solve2 [0, 1, 2]) [1..6] `shouldBe` [12, 21, 102, 120, 201, 210]

  describe "factoradic" $
    it "階乗進数の各桁の数" $
      map P.factoradic [0..23] `shouldBe` [[], [1],
                                           [1,0], [1,1], [2,0], [2,1],
                                           [1,0,0], [1,0,1], [1,1,0], [1,1,1], [1,2,0], [1,2,1],
                                           [2,0,0], [2,0,1], [2,1,0], [2,1,1], [2,2,0], [2,2,1],
                                           [3,0,0], [3,0,1], [3,1,0], [3,1,1], [3,2,0], [3,2,1]]

  describe "nthPermutation" $ do
    it "N番目の順列 - 1" $
      P.nthPermutation "" 1 `shouldBe` ""
    it "N番目の順列 - 2" $
      map (P.nthPermutation [1::Int, 2]) [1, 2] `shouldBe` [[1, 2], [2, 1]]
    it "N番目の順列 - 3" $
      map (P.nthPermutation [0::Int, 1, 2]) [1..6] `shouldBe` [[0,1,2],[0,2,1],[1,0,2],[1,2,0],[2,0,1],[2,1,0]]

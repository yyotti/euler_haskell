module P018Spec where

import qualified P018 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solveBasic" $ do
    it "ルート上の数の和の最大値 - 1" $
      P.solveBasic [] `shouldBe` 0

    it "ルート上の数の和の最大値 - 2" $
      P.solveBasic [[1]] `shouldBe` 1

    it "ルート上の数の和の最大値 - 3" $
      P.solveBasic [[1], [2, 3]] `shouldBe` 4

    it "ルート上の数の和の最大値 - 4" $
      P.solveBasic [[1], [2, 3], [2, 3, 1]] `shouldBe` 7

    it "ルート上の数の和の最大値 - 5" $
      P.solveBasic [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]] `shouldBe` 23

  describe "solve" $ do
    it "ルート上の数の和の最大値 - 1" $
      P.solve [] `shouldBe` 0

    it "ルート上の数の和の最大値 - 2" $
      P.solve [[1]] `shouldBe` 1

    it "ルート上の数の和の最大値 - 3" $
      P.solve [[1], [2, 3]] `shouldBe` 4

    it "ルート上の数の和の最大値 - 4" $
      P.solve [[1], [2, 3], [2, 3, 1]] `shouldBe` 7

    it "ルート上の数の和の最大値 - 5" $
      P.solve [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]] `shouldBe` 23

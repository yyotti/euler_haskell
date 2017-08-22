module P002Spec where

import qualified P002 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solveBasic" $ do
    it "N以下のフィボナッチ数列の偶数の項の和 - 1" $
      map P.solveBasic [1..10] `shouldBe` [0, 2, 2, 2, 2, 2, 2, 10, 10, 10]

    it "N以下のフィボナッチ数列の偶数の項の和 - 2" $
      map P.solveBasic [100, 1000] `shouldBe` [44, 798]

  describe "solve" $ do
    it "N以下のフィボナッチ数列の偶数の項の和 - 1" $
      map P.solve [1..10] `shouldBe` [0, 2, 2, 2, 2, 2, 2, 10, 10, 10]

    it "N以下のフィボナッチ数列の偶数の項の和 - 2" $
      map P.solve [100, 1000] `shouldBe` [44, 798]

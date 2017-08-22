module P006Spec where

import qualified P006 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solveBasic" $ do
    it "N以下の自然数の二乗和と和の二乗の差 - 1" $
      map P.solveBasic [1..5] `shouldBe` [0, 4, 22, 70, 170]

    it "N以下の自然数の二乗和と和の二乗の差 - 2" $
      P.solveBasic 10 `shouldBe` 2640

  describe "solve" $ do
    it "N以下の自然数の二乗和と和の二乗の差 - 1" $
      map P.solve [1..5] `shouldBe` [0, 4, 22, 70, 170]

    it "N以下の自然数の二乗和と和の二乗の差 - 2" $
      P.solve 10 `shouldBe` 2640

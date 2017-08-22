module P010Spec where

import qualified P010 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solveBasic" $ do
    it "N以下の素数の和 - 1" $
      map P.solveBasic [-1,0..5] `shouldBe` [0, 0, 0, 2, 5, 5, 10]

    it "N以下の素数の和 - 2" $
      P.solveBasic 10 `shouldBe` 17

  describe "sieve" $
    it "N以下の素数列 - 1" $
      P.sieve 30 `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

  describe "solve" $ do
    it "N以下の素数の和 - 1" $
      map P.solve [-1,0..5] `shouldBe` [0, 0, 0, 2, 5, 5, 10]

    it "N以下の素数の和 - 2" $
      P.solve 10 `shouldBe` 17

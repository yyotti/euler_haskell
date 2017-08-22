module P001Spec where

import qualified P001 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solveBasic" $ do
    it "N未満、かつ3もしくは5で割り切れる数の和 - 1" $
      map P.solveBasic [1..10] `shouldBe` [0, 0, 0, 3, 3, 8, 14, 14, 14, 23]

    it "N未満、かつ3もしくは5で割り切れる数の和 - 2" $
      P.solveBasic 20 `shouldBe` (78 :: Int)

  describe "solve" $ do
    it "N未満、かつ3もしくは5で割り切れる数の和 - 1" $
      map P.solve [1..10] `shouldBe` [0, 0, 0, 3, 3, 8, 14, 14, 14, 23]

    it "N未満、かつ3もしくは5で割り切れる数の和 - 2" $
      P.solve 20 `shouldBe` (78 :: Int)

module P014Spec where

import qualified P014 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "collatz" $
    it "次のコラッツ数" $
      map P.collatz [1,2,3,4,5,6] `shouldBe` [4,1,10,2,16,3]

  describe "collatzChainLen" $
    it "Nのコラッツ数列の長さ" $
      map P.collatzChainLen [1, 2, 3, 13] `shouldBe` [0, 1, 7, 9]

  describe "solveBasic" $
    it "N以下の数でコラッツ数列が最も長くなるもの" $
      map P.solveBasic [1, 2, 3, 4, 10] `shouldBe` [1, 2, 3, 3, 9]

  describe "collatzChainLenMem" $
    it "Nのコラッツ数列の長さ(メモ化)" $
      map P.collatzChainLenMem [1, 2, 3, 13] `shouldBe` [0, 1, 7, 9]

  describe "solve" $
    it "N以下の数でコラッツ数列が最も長くなるもの" $
      map P.solve [1, 2, 3, 4, 10] `shouldBe` [1, 2, 3, 3, 9]

module P012Spec where

import qualified P012 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "triangles" $
    it "三角数の数列" $
      take 10 P.triangles `shouldBe` [1,3,6,10,15,21,28,36,45,55]

  describe "solveBasic" $
    it "約数の個数がN個を超える最初の三角数" $
      map P.solveBasic [0,1,2,5] `shouldBe` [1,3,6,28]

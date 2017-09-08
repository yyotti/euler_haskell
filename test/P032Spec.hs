module P032Spec where

import qualified P032 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = -- do
  describe "solveBasic" $
    it "1からNまでのパンデジタルになる掛け算の積の和" $
      map P.solveBasic [1..5] `shouldBe` [0, 0, 0, 12, 52]

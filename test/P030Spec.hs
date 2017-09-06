module P030Spec where

import qualified P030 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = -- do
  describe "solveBasic" $
    it "各桁の数字をN乗した和が自分自身になる数の和" $
      map P.solveBasic [1, 2, 4] `shouldBe` [44, 0, 19316]

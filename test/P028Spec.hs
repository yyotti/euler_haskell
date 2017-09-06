module P028Spec where

import qualified P028 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = -- do
  describe "solveBasic" $
    it "サイズNの対角線上の数の和" $
      map P.solveBasic [1,3..7] `shouldBe` [1,25,101,261]

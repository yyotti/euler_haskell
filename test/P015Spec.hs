module P015Spec where

import qualified P015 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "solveBasic" $
    it "NxNのマス目の全経路" $
      map P.solveBasic [1, 2, 3, 4, 5] `shouldBe` [2, 6, 20, 70, 252]

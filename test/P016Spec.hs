module P016Spec where

import qualified P016 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "solveBasic" $
    it "2^Nの各桁の和" $
      map P.solveBasic [0, 1, 2, 3, 4, 5, 6, 7, 15] `shouldBe` [1, 2, 4, 8, 7, 5, 10, 11, 26]

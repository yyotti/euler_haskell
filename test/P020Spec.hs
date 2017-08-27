module P020Spec where

import qualified P020 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "solveBasic" $
    it "N!の各桁の和" $
      map P.solveBasic [1..10] `shouldBe` [1, 2, 6, 6, 3, 9, 9, 9, 27, 27]

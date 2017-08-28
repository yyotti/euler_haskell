module P021Spec where

import qualified P021 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "solveBasic" $
    it "N未満の友愛数の和" $
      map P.solveBasic [0,1,2,500,1000,1500] `shouldBe` [0,0,0,504,504,2898]

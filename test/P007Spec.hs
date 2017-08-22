module P007Spec where

import qualified P007 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "solveBasic" $
    it "指定された個数目の素数" $
      map P.solveBasic [1..6] `shouldBe` [2::Integer, 3, 5, 7, 11, 13]

module P019Spec where

import qualified P019 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "solveBasic" $
    it "20世紀内の月初の日曜日の数" $
      P.solveBasic `shouldBe` 171

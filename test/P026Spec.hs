module P026Spec where

import qualified P026 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "rec" $
    it "小数の循環部" $
      map P.rec [1..13] `shouldBe` [[],[],[3],[],[],[6],[1,4,2,8,5,7],[],[1],[],[0,9],[3],[0,7,6,9,2,3]]

  describe "solveBasic" $
    it "分母がN未満の分数のうち循環部の長さが最長の数" $
      map P.solveBasic [10,20..100] `shouldBe` [7,19,29,29,47,59,61,61,61,97]

  describe "solve" $
    it "分母がN未満の分数のうち循環部の長さが最長の数" $
      map P.solve [10,20..100] `shouldBe` [7,19,29,29,47,59,61,61,61,97]

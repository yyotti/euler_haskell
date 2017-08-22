module P005Spec where

import qualified P005 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solveBasic" $
    it "1以上N以下の全ての整数で割り切れる最小の数" $
      map P.solveBasic [1..10] `shouldBe` [1, 2, 6, 12, 60, 60, 420, 840, 2520, 2520]

  describe "solve" $
    it "1以上N以下の全ての整数で割り切れる最小の数" $
      map P.solve [1..10] `shouldBe` [1, 2, 6, 12, 60, 60, 420, 840, 2520, 2520]

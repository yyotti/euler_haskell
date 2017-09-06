module P029Spec where

import qualified P029 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = -- do
  describe "solveBasic" $ do
    it "2 <= a <= M, 2 <= b <= Nの時にa^bで生成できる数の個数 - 1" $
      map (uncurry P.solveBasic) [(2,2), (2,3), (3,2), (3,3), (3,4), (4,3), (4,4), (5,5)] `shouldBe` [1,2,2,4,6,6,8,15]
    it "2 <= a <= M, 2 <= b <= Nの時にa^bで生成できる数の個数 - 2" $
      P.solveBasic 20 20 `shouldBe` 324

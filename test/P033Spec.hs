module P033Spec where

import qualified P033 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solveBasic" $
    it "勘違いの約分をしても正しくなる分数の積の分母" $
      P.solveBasic `shouldBe` 100

  describe "solve" $
    it "勘違いの約分をしても正しくなる分数の積の分母" $
      P.solve `shouldBe` 100

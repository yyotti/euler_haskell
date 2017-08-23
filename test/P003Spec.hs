module P003Spec where

import qualified P003 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "solveBasic" $ do
    it "Nの素因数の最大値 - 1" $
      map P.solveBasic [2..6] `shouldBe` [2, 3, 2, 5, 3]

    it "Nの素因数の最大値 - 2" $
      P.solveBasic 13195 `shouldBe` 29

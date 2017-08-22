module P009Spec where

import qualified P009 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solveBasic" $ do
    it "和に該当する組み合わせがあるならその積の最大値" $
      map P.solveBasic [12, 30, 90] `shouldBe` [60, 780, 21060]

    it "和に該当する組み合わせがないなら0" $
      map P.solveBasic [0, 1, 13] `shouldBe` [0, 0, 0]

  describe "solve" $ do
    it "和に該当する組み合わせがあるならその積の最大値" $
      map P.solve [12, 30, 90] `shouldBe` [60, 780, 21060]

    it "和に該当する組み合わせがないなら0" $
      map P.solve [0, 1, 13] `shouldBe` [0, 0, 0]

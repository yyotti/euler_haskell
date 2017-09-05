module P025Spec where

import qualified P025 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solveBasic" $
    it "フィボナッチ数列で最初にN桁になるインデックス" $
      map P.solveBasic [1,2,3,4] `shouldBe` [1,7,12,17]

  describe "solve" $
    it "フィボナッチ数列で最初にN桁になるインデックス" $
      map P.solve [1,2,3,4] `shouldBe` [1,7,12,17]

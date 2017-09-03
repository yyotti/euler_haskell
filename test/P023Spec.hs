module P023Spec where

import qualified P023 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  let t = True
  let f = False
  describe "isAbundant" $
    it "過剰数判定" $
      map P.isAbundant [0..20] `shouldBe` [f,f,f,f,f,f,f,f,f,f,f,f,t,f,f,f,f,f,t,f,t]

  describe "solveBasic" $
    it "2つの過剰数の和で表せない数の和" $
      P.solveBasic `shouldBe` 4179871

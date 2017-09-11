module P034Spec where

import qualified P034 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = -- do
  -- NOTE: 長いのでコメントアウト
  -- describe "solveBasic" $
  --   it "各桁の階乗の和が自分自身と等しくなる数の和" $
  --     P.solveBasic `shouldBe` 40730

  describe "solve" $
    it "各桁の階乗の和が自分自身と等しくなる数の和" $
      P.solve `shouldBe` 40730


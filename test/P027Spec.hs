module P027Spec where

import qualified P027 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = -- do
  -- NOTE: 遅すぎるのでコメントアウトしている
  -- describe "solveBasic" $
  --   it "最も多くの素数が生成できる係数a,bの積" $
  --     P.solveBasic `shouldBe` -59231

  describe "solve" $
    it "最も多くの素数が生成できる係数a,bの積" $
      P.solve `shouldBe` -59231

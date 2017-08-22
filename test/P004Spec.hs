module P004Spec where

import qualified P004 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isPalindrome" $
    it "回文数判定" $ do
      let t = True
      let f = False
      let input = [0, 1, 9, 10, 11, 12, 21, 22, 100, 101, 111, 112, 121, 1001, 1010, 2022, 3303, 4444, 4554]
      let expected = [t, t, t, f, t, f, f, t, f, t, t, f, t, t, f, f, f, t, t]
      map P.isPalindrome input `shouldBe` expected

  describe "solveBasic" $
    it "N桁の数を掛け合わせてできる最大の回文数" $
      map P.solveBasic [1, 2] `shouldBe` [9, 9009]

  describe "solve" $
    it "N桁の数を掛け合わせてできる最大の回文数" $
      map P.solve [1, 2] `shouldBe` [9, 9009]

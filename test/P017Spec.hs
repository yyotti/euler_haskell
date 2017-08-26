module P017Spec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "solveBasic" $
    it "英語表記にした場合の文字数の総和" $ pendingWith "つまらんので無視"

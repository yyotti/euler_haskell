module P022Spec where

import qualified P022 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "loadNames" $ do
    it "空のファイルなら空リスト" $
      P.loadNames "testdata/P022/empty.txt" `shouldReturn` []
    it "名前のリスト - 1" $
      P.loadNames "testdata/P022/names1.txt" `shouldReturn` [ "MICHEL" ]
    it "名前のリスト - 2" $
      P.loadNames "testdata/P022/names2.txt" `shouldReturn` [ "MICHEL", "PETER" ]

  describe "nameScore" $ do
    it "空文字列なら0" $
      P.nameScore "" `shouldBe` 0
    it "空でないならスコア - 1" $
      P.nameScore "A" `shouldBe` 1
    it "空でないならスコア - 2" $
      P.nameScore "MICHEL" `shouldBe` 50

  describe "solveBasic" $ do
    it "空のファイルなら0" $
      P.solveBasic "testdata/P022/empty.txt" `shouldReturn` 0
    it "名前のリスト - 1" $
      P.solveBasic "testdata/P022/names1.txt" `shouldReturn` 50
    it "名前のリスト - 2" $
      P.solveBasic "testdata/P022/names2.txt" `shouldReturn` 178

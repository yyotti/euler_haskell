module P035Spec where

import qualified P035 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "rotate" $
    it "循環させる" $ do
      let input = [1, 10, 11, 12, 100, 101, 110, 211]
      let expected = [[1],
                      [10, 1],
                      [11, 11],
                      [12, 21],
                      [100, 1, 10],
                      [101, 11, 110],
                      [110, 101, 11],
                      [211, 112, 121]]
      map P.rotate input `shouldBe` expected

  describe "solveBasic" $
    it "N以下の循環素数の個数" $
      map P.solveBasic [1,10,20,50,100] `shouldBe` [0,4,7,9,13]

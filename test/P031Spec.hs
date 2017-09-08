module P031Spec where

import qualified P031 as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = -- do
  describe "solveBasic" $
    it "硬貨で合計Nを作る組み合わせの数" $
      map (uncurry P.solveBasic) [(0, [1, 2]),
                                  (1, []), (1, [2]), (1, [1, 2]),
                                  (2, [1, 2]),
                                  (3, [1, 2]),
                                  (4, [1, 2]), (4, [1, 2, 3])] `shouldBe` [1, 0, 0, 1, 2, 2, 3, 4]

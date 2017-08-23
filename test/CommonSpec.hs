module CommonSpec where

import qualified Common as C
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $
    it "0,1から始まるフィボナッチ数列" $
      take 10 C.fib `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

  describe "primes" $
    it "素数列" $
      take 10 C.primes `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

  describe "isPrime" $ do
    it "1以下の場合はFalse" $
      map C.isPrime [-1, 0, 1] `shouldBe` [False, False, False]
    it "2以上の場合は素数判定" $ do
      let t = True
      let f = False
      map C.isPrime [2..20] `shouldBe` [t,t,f,t,f,t,f,f,f,t,f,t,f,f,f,t,f,t,f]

  describe "primeFactors" $ do
    it "1以下の場合は空" $
      map C.primeFactors [-1, 0, 1] `shouldBe` [[], [], []]
    it "2以上の場合は素因数分解 - 1" $
      map C.primeFactors [2..9] `shouldBe`
        [[(1,2)], [(1,3)], [(2,2)], [(1,5)], [(1,2), (1,3)], [(1,7)], [(3,2)], [(2,3)]]
    it "2以上の場合は素因数分解 - 2" $
      C.primeFactors 13195 `shouldBe` [(1,5),(1,7),(1,13),(1,29)]

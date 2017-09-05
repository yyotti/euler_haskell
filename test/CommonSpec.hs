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

  describe "digits" $ do
    it "負の場合は空" $
      C.digits (-1::Int) `shouldBe` []
    it "1以上の場合は桁に分解 - 1" $
      map C.digits [1::Int, 10, 11, 200, 201] `shouldBe` [[1], [1, 0], [1, 1], [2, 0, 0], [2, 0, 1]]
    it "1以上の場合は桁に分解 - 2" $
      C.digits (12345::Integer) `shouldBe` [1..5]

  describe "permutation" $
    it "順列 nPr" $
      map (uncurry C.permutation) [(-1,-2), (-1,-1), (1,-1), (-1,1),
                         (0,0),
                         (1,0), (1,1), (1,2),
                         (2,0), (2,1), (2,2), (2,3),
                         (3,0), (3,1), (3,2), (3,3), (3,4),
                         (4,0), (4,1), (4,2), (4,3), (4,4), (4,5)]
                         `shouldBe`
                         [0, 0, 0, 0,
                          1,
                          1, 1, 0,
                          1, 2, 2, 0,
                          1, 3, 6, 6, 0,
                          1, 4, 12, 24, 24, 0]

  describe "fact" $
    it "階乗 n!" $
      map C.fact [-1, 0, 1, 2, 3, 4, 5] `shouldBe` [0, 1, 1, 2, 6, 24, 120]

  describe "combination" $
    it "組み合わせ nCr" $
      map (uncurry C.combination) [(-1,-1), (1,-1), (-1,1),
                                   (0,0),
                                   (1,0), (1,1), (1,2),
                                   (2,0), (2,1), (2,2), (2,3),
                                   (3,0), (3,1), (3,2), (3,3), (3,4),
                                   (4,0), (4,1), (4,2), (4,3), (4,4), (4,5),
                                   (5,0), (5,1), (5,2), (5,3), (5,4), (5,5), (5,6)]
                                  `shouldBe`
                                  [0, 0, 0,
                                   1,
                                   1, 1, 0,
                                   1, 2, 1, 0,
                                   1, 3, 3, 1, 0,
                                   1, 4, 6, 4, 1, 0,
                                   1, 5, 10, 10, 5, 1, 0]

  describe "sumDivisors" $
    it "約数の和" $
      map C.sumDivisors [0,1,2,3,4,24] `shouldBe` [1,1,3,4,7,60]

  describe "divisors" $
    it "約数" $
      map C.divisors [1,2,3,4,24] `shouldBe` [[1],[1,2],[1,3],[1,2,4],[1,2,3,4,6,8,12,24]]

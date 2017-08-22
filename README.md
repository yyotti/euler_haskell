# euler_haskell

## 解法
確実に解けるようにベーシックな解答を1つと、ベーシックに比べたら高速に解ける方法を1つ。
ただし、ベーシックな方法があまりに時間がかかるようなら省略する（コメントにするなりしてソースには残す）。

テストでは、問題からは想定されないような不正な入力に対してはチェックしないことにする。

## 実行
各問題について下記のコマンドで実行する。
```sh
cd $REPO_ROOT
runhaskell -isrc src/PXXX.hs
```

## テスト
まずHSpecをCabalでインストールする。
*Bash on WindowsではStackが遅すぎるのでCabalを使っているが、改善されたらStackでやるように修正する。*
```sh
cabal update && cabal install hspec  # 公式にあるものそのまま
```
また、`$HOME/.cabal/bin`を`PATH`に入れておく。

それぞれの問題単体でのテストは
```sh
cd $REPO_ROOT
runhaskell -isrc test/PXXXSpec.hs
```
で実行できる。

全てのテストを一括実行するなら
```sh
cd $REPO_ROOT
runhaskell -isrc -itest test/Spec.hs
```
で実行できる。

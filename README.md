# jcc
JSON Constitution Checker

## 概要
JSONの構造が指定した構造に対応しているか判定するチェッカー

## ビルド
ghc --make jcc

## サンプル
jcc -c sample/constitution.json -t sample/target.json

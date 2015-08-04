-- JSON Constitution Checker

import System.Environment
import Control.Exception
import Data.Either
import Data.List
import Data.Maybe
import Json
import Match
import Message

main = do
  args <- getArgs
  let fileNames = getFileNames args
  if isLeft fileNames
    then
      putStrLn $ head $ lefts (fileNames:[])
    else do
      let (constitutionFileName, targetFileNames) = head $ rights (fileNames:[])

      constitution <- catch (readFile constitutionFileName) (readErrorHander constitutionFileName)
      targets <- mapM (\fileName -> catch (readFile fileName) (readErrorHander fileName)) targetFileNames

      putStrLn $ match $ zip (constitutionFileName:targetFileNames) $ map parseJson (constitution:targets)

{-
 - コマンド引数からファイル名を取得する
 -
 - [String]                         コマンド引数
 - Either String (String, [String]) エラーメッセージか解析情報ファイル名と解析対象のファイル名のペア
 -}
getFileNames :: [String] -> Either String (String, [String])
getFileNames xs = case parseArgs xs of
                  Left m    ->  Left m
                  Right ys  ->  let (_, constitutionFileName:_) = fromJust $ find (\(x, _) -> x == "-c") ys -- parseArgsで必要な要素は揃っていることはチェック済み
                                    (_, targetFileNames)        = fromJust $ find (\(x, _) -> x == "-t") ys
                                in  Right (constitutionFileName, targetFileNames)

{-
 - 構成が正しいか判定する
 -
 - [(String, Either String JsonObject)] ファイル名とJSONのパース結果のタプルリスト
 -}
match :: [(String, Either String JsonObject)] -> String
match xs  = case find (\(n, e) -> isLeft e) xs of
            Just (n, e) ->  emParseError n e
            Nothing     ->  let (_, Right c) = xs!!0
                                (tn, Right t) = xs!!1
                            in  case matchConstitution ((\x -> (JsonObject x)) t) c of
                                ""  -> tn ++ " is match."
                                e   -> tn ++ " is not match : " ++ e

{-
 - コマンド引数の配列を解析する
 -
 - [String]                         コマンド引数
 - Either String (String, [String]) エラーメッセージか構成情報ファイル名と対象ファイル名の配列のペア
 -}
parseArgs :: [String] -> Either String [(String, [String])]
parseArgs xs  = case divideOptions xs of
                []            ->  Left "Need options -c, -t"
                options       ->  case filter (/= mMatch) $ map isAvailableOption options of
                                  []    -> let c = any (\(x, _) -> x == "-c") options
                                               t = any (\(x, _) -> x == "-t") options
                                           in  if c && t then Right options else Left "Need options -c, -t"
                                  y:ys  -> Left y

{-
 - コマンド引数をオプションとその引数に分解する
 -
 - [String]             コマンド引数
 - [(String, [String])] オプション名とその引数のペアのリスト
 -}
divideOptions :: [String] -> [(String, [String])]
divideOptions []  = []
divideOptions xs  = let (options, other1) = break (not . isOption) xs
                    in  let (args, other2) = break isOption other1
                        in  let option = if options == [] then "" else head options
                            in  (option, args):(divideOptions other2)

-- オプションの文字列か判定する
isOption :: String -> Bool
isOption ('-':xs) = True
isOption _        = False

-- 有効なオプションか判定する
isAvailableOption :: (String, [String]) -> String
isAvailableOption ("-c", xs)  = if length xs == 1 then mMatch else "-c option receivable argument count is one."
isAvailableOption ("-t", xs)  = if length xs >= 1 then mMatch else "-t option need one or more argument count."
isAvailableOption _           = "Has unkown option."

-- ファイル読み込みエラー処理
readErrorHander :: String -> IOError -> IO String
readErrorHander fileName error = do
  putStrLn $ "Can not open File \"" ++ fileName ++ "\""
  return ""


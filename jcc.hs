-- JSON Constitution Checker

import System.Environment
import Control.Exception
import Data.Either
import Data.List
import Json
import Match
import Message

main = do
  c:constitutionFileName:t:targetFileName:_ <- getArgs
  constitution <- catch (readFile constitutionFileName) (readErrorHander constitutionFileName)
  target <- catch (readFile targetFileName) (readErrorHander targetFileName)

  putStrLn $ match $ zip (constitutionFileName:targetFileName:[]) $ map parseJson (constitution:target:[])

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
                ("", args):_  ->  Left ("Unkown option " ++ show args)
                options       ->  case filter (/= mMatch) $ map isAvailableOption options of
                                []    -> Right options
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
isAvailableOption (x, _)      = x ++ " is unkown option."

-- ファイル読み込みエラー処理
readErrorHander :: String -> IOError -> IO String
readErrorHander fileName error = do
  putStrLn $ "Can not open File \"" ++ fileName ++ "\""
  return ""


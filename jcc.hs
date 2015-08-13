-- JSON Constitution Checker
import System.Environment
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Json
import Match
import Message

{-
 - エントリーポイント
 -}
main :: IO ()
main = do
  args <- getArgs
  let fileNames = getFileNames args
  putStrLn =<< (either pure matchFiles fileNames) `catch` ((\x -> return "") :: SomeException -> IO String)

{-
 - ファイルを読み込み、構成が正しいか判断する
 -
 - (String, [String]) 構成情報ファイル名と対象ファイル名リストのペア
 - IO String          判断結果のメッセージ
 -}
matchFiles :: (String, [String]) -> IO String
matchFiles (cPath, tPaths)  = either
                                (\x -> return $ emParseError cPath x)
                                (\constitution -> do
                                  targets <- mapM (\fileName -> onException (readFile fileName) (readErrorHander fileName)) tPaths
                                  return $ foldl1 (\x y -> x ++ "\n" ++ y) $ map eitherValue $ map (\(tFileName, tFile) -> match tFileName constitution <$> parseJson tFile) $ zip tPaths targets
                                ) =<< parseJson <$> onException (readFile cPath) (readErrorHander cPath)

-- Either の要素を取り出す
eitherValue :: Either a a -> a
eitherValue (Left x)  = x
eitherValue (Right x) = x

{-
 - コマンド引数からファイル名を取得する
 -
 - [String]                   コマンド引数
 - Fragile (String, [String]) エラーメッセージか解析情報ファイル名と解析対象のファイル名のペア
 -}
getFileNames :: [String] -> Fragile (String, [String])
getFileNames xs = (\ys -> do
                    let (_, constitutionFileName:_) = fromJust $ find (\(x, _) -> x == "-c") ys -- parseArgsで必要な要素は揃っていることはチェック済み
                        (_, targetFileNames)        = fromJust $ find (\(x, _) -> x == "-t") ys
                    (constitutionFileName, targetFileNames)
                  ) <$> parseArgs xs

{-
 - 構成が正しいか判定する
 -
 - [(String, Fragile JsonObject)] ファイル名とJSONのパース結果のタプルリスト
 -}
match :: String -> JsonObject -> JsonObject -> String
match n c t = case matchConstitution ((\x -> (JsonObject x)) t) c of
              ""  -> n ++ " is match."
              e   -> n ++ " is not match : " ++ e

{-
 - コマンド引数の配列を解析する
 -
 - [String]                   コマンド引数
 - Fragile (String, [String]) エラーメッセージか構成情報ファイル名と対象ファイル名の配列のペア
 -}
parseArgs :: [String] -> Fragile [(String, [String])]
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
readErrorHander :: String -> IO ()
readErrorHander fileName = putStrLn $ "Can not open File \"" ++ fileName ++ "\""


module Json (
  JsonPair,
  JsonValue(..),
  JsonString,
  JsonObject,
  JsonArray,
  parseJsonFile,
  bracketContent,
  valueToText
) where 

import System.IO
import Control.Applicative
import Control.Exception
import Data.Either
import Message

-- JSONデータ
type JsonPair = (String, JsonValue)
type JsonBool = Bool
type JsonString = String
type JsonObject = [JsonPair]
type JsonArray = [JsonValue]
data JsonValue = JsonBool Bool | JsonNumber Float | JsonString String | JsonObject JsonObject | JsonArray [JsonValue] | JsonNull deriving (Show)

{-
 - ファイルから読み込んだJSONテキストをパースする
 -
 - FilePath                 ファイルのパス
 - IO (Fragile JsonObject)  エラーメッセージかパースされたJSONのオブジェクトのIO
 -}
parseJsonFile :: FilePath -> IO (Fragile JsonObject)
parseJsonFile path  = either
                        (\m -> Left $ emParseError path m)
                        Right
                        <$> parseJson <$> onException (readFile path) (putStrLn $ emCanNotOpenFile path)

{-
 - JSONテキストをパースする
 -
 - String             JSONテキスト
 - Fragile JsonObject エラーメッセージかパースされたJSONのオブジェクト
 -}
parseJson :: String -> Fragile JsonObject
parseJson text = parseObject $ removeWhiteSpace text

{-
 - 囲まれている要素を取り出す
 -
 - String         要素を取り出される文字列
 - Char           囲む開始文字
 - Char           囲む終了文字
 - Fragile String エラーメッセージか取り出された文字列
 -}
bracketContent :: String -> Char -> Char -> Fragile String
bracketContent text sb eb | sb == eb  = maybe (emlNonBracketPair sb eb) Right $ sameBracketContent text sb
                          | otherwise = maybe (emlNonBracketPair sb eb) Right $ bracketContentLevel text sb eb []

{-
 - 囲まれている階層構造の要素を取り出す
 -
 - String       要素を取り出される文字列
 - Char         囲む開始文字
 - Char         囲む終了文字
 - Maybe String 取り出された文字列
 -}
bracketContentLevel :: String -> Char -> Char -> [Char] -> Maybe String
bracketContentLevel "" _ _ _                            = Nothing
bracketContentLevel (x:xs) sb eb []         | x == sb   = bracketContentLevel xs sb eb (eb:[])
                                            | otherwise = bracketContentLevel xs sb eb []
bracketContentLevel (x:xs) sb eb (s:[])     | x == sb   = (x:) <$> bracketContentLevel xs sb eb (eb:s:[])
                                            | x == s    = Just ""
                                            | otherwise = (x:) <$> bracketContentLevel xs sb eb (s:[])
bracketContentLevel (x:xs) sb eb (s:stack)  | x == sb   = (x:) <$> bracketContentLevel xs sb eb (eb:s:stack)
                                            | x == s    = (x:) <$> bracketContentLevel xs sb eb stack
                                            | otherwise = (x:) <$> bracketContentLevel xs sb eb (s:stack)

-- 同じ文字で囲まれている要素を取り出す
sameBracketContent :: String -> Char -> Maybe String
sameBracketContent (x:xs) c | x == c    = sameBracketContentInBracket xs c
                            | otherwise = sameBracketContent xs c

sameBracketContentInBracket :: String -> Char -> Maybe String
sameBracketContentInBracket ('\\':x:xs) c             = (\y -> '\\':x:y) <$> sameBracketContentInBracket xs c
sameBracketContentInBracket (x:xs) c      | x == c    = Just ""
                                          | otherwise = (x:) <$> sameBracketContentInBracket xs c
sameBracketContentInBracket _ _                       = Nothing

-- キーと値のペアをパースする
parsePair :: String -> Fragile JsonPair
parsePair text  = case break (== ':') text of
                  (keyText, ':':valueText)  ->  (\b -> (\x -> (b, x)) <$> parseValue valueText) =<< bracketContent keyText '"' '"'
                  (text, _)                 ->  emlNonObjectKeyValuePair text

-- 値をパースする
parseValue :: String -> Fragile JsonValue
parseValue text@('{':xs)  = JsonObject <$> parseObject text
parseValue text@('[':xs)  = JsonArray <$> parseArray text
parseValue text@('"':xs)  = JsonString <$> bracketContent text '"' '"'
parseValue "True"         = Right $ JsonBool True
parseValue "False"        = Right $ JsonBool False
parseValue "null"         = Right JsonNull
parseValue ""             = emlNonValue
parseValue text           = Right $ JsonNumber $ read text

-- JSONのオブジェクトをパースする
parseObject :: String -> Fragile JsonObject
parseObject xs  = parseObjectContents =<< (bracketContent xs '{' '}')

{-
 - ',' で区切られた文字列をパースする
 -
 - (String -> Fragile a)  パース関数
 - String                 文字列
 - Fragile a              エラーメッセージかパース結果の要素リスト
 -}
parseCollection :: (String -> Fragile a) -> String -> Fragile [a]
parseCollection f xs  = (\x ->  case partitionEithers $ map f x of
                                ([], y)   -> Right y
                                (y:_, _)  -> Left y
                        ) =<< divideTopLevel xs ','

-- JSONのオブジェクトの要素をパースする
parseObjectContents :: String -> Fragile JsonObject
parseObjectContents xs  = parseCollection parsePair xs

-- JSONの配列をパースする
parseArray :: String -> Fragile [JsonValue]
parseArray xs = case bracketContent xs '[' ']' of
                Right ""  -> Right []                         -- 配列の内容が空の場合は、空配列とする
                x         -> parseCollection parseValue =<< x

-- トップレベルにある文字で文字列を分割する
divideTopLevel :: String -> Char -> Fragile [String]
divideTopLevel xs c = divideTopLevelWork xs c []

divideTopLevelWork :: String -> Char -> [Char] -> Fragile [String]
divideTopLevelWork "" _ (s:stack)                 = emlNonCloseBracket s                                      -- スタック終端文字が残っている場合はエラー
divideTopLevelWork "" _ _                         = Right [""]
divideTopLevelWork ('{':xs) c stack               = addFirstHead '{' <$> divideTopLevelWork xs c ('}':stack)  -- オブジェクトの開始文字が見つかったので、スタックに終了文字を追加
divideTopLevelWork ('[':xs) c stack               = addFirstHead '[' <$> divideTopLevelWork xs c (']':stack)  -- 配列の開始文字が見つかったので、スタックに終了文字を追加
divideTopLevelWork (x:xs) c []        | x == c    = ("":) <$> divideTopLevelWork xs c []                      -- 目的の文字が見つかった
                                      | otherwise = addFirstHead x <$> divideTopLevelWork xs c []
divideTopLevelWork (x:xs) c (s:stack) | x == s    = addFirstHead x <$> divideTopLevelWork xs c stack          -- スタックにある文字が見つかったので、スタックから削除
                                      | otherwise = addFirstHead x <$> divideTopLevelWork xs c (s:stack)

{-
 - 二重リストの先頭に要素を追加する
 -
 - a      追加する要素
 - [[a]]  対象のEither
 - [[a]]  追加された結果
 -}
addFirstHead :: a -> [[a]] -> [[a]]
addFirstHead a (x:xs) = (a:x):xs

{-
 - JSONテキストから不要なスペースを削除する
 -
 - String 元の文字列
 - String スペースの削除された文字列
 -}
removeWhiteSpace :: String -> String
removeWhiteSpace ""         = ""
removeWhiteSpace ('"':xs)   = '"':skipJsonString xs
removeWhiteSpace (' ':xs)   = removeWhiteSpace xs
removeWhiteSpace ('\r':xs)  = removeWhiteSpace xs
removeWhiteSpace ('\n':xs)  = removeWhiteSpace xs
removeWhiteSpace ('\t':xs)  = removeWhiteSpace xs
removeWhiteSpace (x:xs)     = x:removeWhiteSpace xs

-- JSONの文字列をスキップする
skipJsonString :: String -> String
skipJsonString ""           = ""
skipJsonString ('"':xs)     = '"':removeWhiteSpace xs
skipJsonString ('\\':x:xs)  = '\\':x:skipJsonString xs
skipJsonString (x:xs)       = x:skipJsonString xs

{-
 - JsonValueをJSONのテキストに変換する
 -
 - JsonValue  変換元の値
 - String     JSONテキスト
 -}
valueToText :: JsonValue -> String
valueToText (JsonObject o)  = "{ " ++ (foldl1 (\x y -> x ++ ", " ++ y) $ map pairToText o) ++ " }"
valueToText (JsonNumber n)  = show n
valueToText (JsonString s)  = show s
valueToText (JsonArray a)   = "[ " ++ (foldl1 (\x y -> x ++ ", " ++ y) $ map valueToText a) ++ " ]"
valueToText JsonNull        = "null"
valueToText v               = show v

{-
 - JsonPairをJSONのテキストに変換する
 -
 - JsonPair 変換元のペア
 - String   JSONテキスト
 -}
pairToText :: JsonPair -> String
pairToText (key, value) = key ++ ": " ++ valueToText value

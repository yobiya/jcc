module Json (
  JsonPair,
  JsonValue(..),
  JsonString,
  JsonObject,
  JsonArray,
  parseJson,
  jsonObjectContents,
  bracketContent 
) where 

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
 - JSONテキストをパースする
 -
 - String     JSONテキスト
 - JsonObject パースされたJSONのオブジェクト
 -}
parseJson :: String -> Either String JsonObject
parseJson text = parseObject $ removeWhiteSpace text

{-
 - JsonValueの型がJsonObjectの場合に要素を取り出す
 -
 - JsonValue  判定されるJsonValue
 - [JsonPair] 取り出されたJsonObjectの内容
 -}
jsonObjectContents :: JsonValue -> [JsonPair]
jsonObjectContents (JsonObject o) = o
jsonObjectContents _              = []

{-
 - 囲まれている要素を取り出す
 -
 - String       要素を取り出される文字列
 - Char         囲む開始文字
 - Char         囲む終了文字
 - Maybe String 取り出された文字列
 -}
bracketContent :: String -> Char -> Char -> Maybe String
bracketContent text sb eb | sb == eb  = sameBracketContent text sb
                          | otherwise = bracketContentLevel text sb eb []

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
bracketContentLevel (x:xs) sb eb (s:[])     | x == sb   = fmap (\y -> x:y) $ bracketContentLevel xs sb eb (eb:s:[])
                                            | x == s    = Just ""
                                            | otherwise = fmap (\y -> x:y) $ bracketContentLevel xs sb eb (s:[])
bracketContentLevel (x:xs) sb eb (s:stack)  | x == sb   = fmap (\y -> x:y) $ bracketContentLevel xs sb eb (eb:s:stack)
                                            | x == s    = fmap (\y -> x:y) $ bracketContentLevel xs sb eb stack
                                            | otherwise = fmap (\y -> x:y) $ bracketContentLevel xs sb eb (s:stack)

-- 同じ文字で囲まれている要素を取り出す
sameBracketContent :: String -> Char -> Maybe String
sameBracketContent (x:xs) c | x == c    = sameBracketContentInBracket xs c
                            | otherwise = sameBracketContent xs c

sameBracketContentInBracket :: String -> Char -> Maybe String
sameBracketContentInBracket ('\\':x:xs) c             = fmap (\y -> '\\':x:y) $ sameBracketContentInBracket xs c
sameBracketContentInBracket (x:xs) c      | x == c    = Just ""
                                          | otherwise = fmap (\y -> x:y) $ sameBracketContentInBracket xs c
sameBracketContentInBracket _ _                       = Nothing

-- キーと値のペアをパースする
parsePair :: String -> Either String JsonPair
parsePair text  = case break (== ':') text of
                  (_, "")                   ->  Left "error"
                  (keyText, ':':valueText)  ->  maybe (emNonBracketPair '"' '"') (\b -> fmap (\x -> (b, x)) $ parseValue valueText) $ bracketContent keyText '"' '"'

-- 値をパースする
parseValue :: String -> Either String JsonValue
parseValue text@('{':xs)  = fmap (\x -> (JsonObject x)) $ parseObject text
parseValue text@('[':xs)  = fmap (\x -> (JsonArray x)) $ parseArray text
parseValue text@('"':xs)  = maybe (emNonBracketPair '"' '"') (\x -> Right $ JsonString x) $ bracketContent text '"' '"'
parseValue "True"         = Right $ JsonBool True
parseValue "False"        = Right $ JsonBool False
parseValue "null"         = Right JsonNull
parseValue ""             = Left "error"
parseValue text           = Right $ JsonNumber $ read text

-- JSONのオブジェクトをパースする
parseObject :: String -> Either String JsonObject
parseObject xs  = maybe (Left "error") parseObjectContents (bracketContent xs '{' '}')

-- JSONのオブジェクトの要素をパースする
parseObjectContents :: String -> Either String JsonObject
parseObjectContents xs  = case divideTopLevel xs ',' of
                          Left x  ->  Left x
                          Right x ->  case partitionEithers $ map parsePair x of
                                      ([], x)   -> Right x
                                      (x:_, _)  -> Left x

-- JSONの配列をパースする
parseArray :: String -> Either String [JsonValue]
parseArray xs = case bracketContent xs '[' ']' of
                Nothing ->  Left "error"
                Just x  ->  case divideTopLevel x ',' of
                            Left x  ->  Left x
                            Right x ->  case partitionEithers $ map parseValue x of
                                        ([], x)   -> Right x
                                        (x:_, _)  -> Left x

-- トップレベルにある文字で文字列を分割する
divideTopLevel :: String -> Char -> Either String [String]
divideTopLevel xs c = divideTopLevelWork xs c []

divideTopLevelWork :: String -> Char -> [Char] -> Either String [String]
divideTopLevelWork "" _ (s:stack)                 = Left "error"                                                -- スタック終端文字が残っている場合はエラー
divideTopLevelWork "" _ _                         = Right [""]
divideTopLevelWork ('{':xs) c stack               = addRightFirstHead '{' $ divideTopLevelWork xs c ('}':stack) -- オブジェクトの開始文字が見つかったので、スタックに終了文字を追加
divideTopLevelWork ('[':xs) c stack               = addRightFirstHead '[' $ divideTopLevelWork xs c (']':stack) -- 配列の開始文字が見つかったので、スタックに終了文字を追加
divideTopLevelWork (x:xs) c []        | x == c    = fmap (\x -> "":x) $ divideTopLevelWork xs c []              -- 目的の文字が見つかった
                                      | otherwise = addRightFirstHead x $ divideTopLevelWork xs c []
divideTopLevelWork (x:xs) c (s:stack) | x == s    = addRightFirstHead x $ divideTopLevelWork xs c stack         -- スタックにある文字が見つかったので、スタックから削除
                                      | otherwise = addRightFirstHead x $ divideTopLevelWork xs c (s:stack)

{-
 - Right の二重リストの先頭に要素を追加する
 -
 - a              追加する要素
 - Either b [[a]] 対象のEither
 - Either b [[a]] 追加された結果
 -}
addRightFirstHead :: a -> Either b [[a]] -> Either b [[a]]
addRightFirstHead a e = fmap (\(x:xs) -> (a:x):xs) e

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

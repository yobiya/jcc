module Json (
  JsonPair,
  JsonValue(JsonBool, JsonNumber, JsonString, JsonObject, JsonArray, JsonNull),
  JsonString,
  JsonObject,
  JsonArray,
  parseJson,
  jsonObjectContents,
  bracketContent 
) where 

import Data.Either

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
 - String 要素を取り出される文字列
 - Char   囲む開始文字
 - Char   囲む終了文字
 - String 取り出された文字列
 -}
bracketContent :: String -> Char -> Char -> String
bracketContent text sb eb | sb == eb  = sameBracketContent text sb
                          | otherwise = bracketContentLevel text sb eb []

{-
 - 囲まれている階層構造の要素を取り出す
 -
 - String 要素を取り出される文字列
 - Char   囲む開始文字
 - Char   囲む終了文字
 - String 取り出された文字列
 -}
bracketContentLevel :: String -> Char -> Char -> [Char] -> String
bracketContentLevel "" _ _ _                            = ""
bracketContentLevel (x:xs) sb eb []         | x == sb   = bracketContentLevel xs sb eb (eb:[])
                                            | otherwise = ""
bracketContentLevel (x:xs) sb eb (s:[])     | x == sb   = x:(bracketContentLevel xs sb eb (eb:s:[]))
                                            | x == s    = ""
                                            | otherwise = x:(bracketContentLevel xs sb eb (s:[]))
bracketContentLevel (x:xs) sb eb (s:stack)  | x == sb   = x:(bracketContentLevel xs sb eb (eb:s:stack))
                                            | x == s    = x:(bracketContentLevel xs sb eb stack)
                                            | otherwise = x:(bracketContentLevel xs sb eb (s:stack))

-- 同じ文字で囲まれている要素を取り出す
sameBracketContent :: String -> Char -> String
sameBracketContent (x:xs) c | x == c    = sameBracketContentInBracket xs c
                            | otherwise = sameBracketContent xs c

sameBracketContentInBracket :: String -> Char -> String
sameBracketContentInBracket ('\\':x:xs) c             = '\\':x:(sameBracketContentInBracket xs c)
sameBracketContentInBracket (x:xs) c      | x == c    = []
                                          | otherwise = x:(sameBracketContentInBracket xs c)

-- キーと値のペアをパースする
parsePair :: String -> Either String JsonPair
parsePair text  = case break (== ':') text of
                    (_, "")                   ->  Left "error"
                    (keyText, ':':valueText)  ->  takeLeft (\x -> Right (bracketContent keyText '"' '"', x)) $ parseValue valueText

-- 値をパースする
parseValue :: String -> Either String JsonValue
parseValue text@('{':xs)  = takeLeft (\x -> Right $ otov x) $ parseObject text
parseValue text@('[':xs)  = takeLeft (\x -> Right $ atov x) $ parseArray text
parseValue text@('"':xs)  = Right $ JsonString $ bracketContent text '"' '"'
parseValue "True"         = Right $ JsonBool True
parseValue "False"        = Right $ JsonBool False
parseValue "null"         = Right JsonNull
parseValue ""             = Left "error"
parseValue text           = Right $ JsonNumber $ read text

-- JSONのオブジェクトをパースする
parseObject :: String -> Either String JsonObject
parseObject xs  = parseObjectContents $ bracketContent xs '{' '}'

-- JSONのオブジェクトの要素をパースする
parseObjectContents :: String -> Either String JsonObject
parseObjectContents xs  = case partitionEithers $ map parsePair $ divideTopLevel xs ',' of
                          ([], x)   -> Right x
                          (x:xs, _) -> Left x

-- JSONの配列をパースする
parseArray :: String -> Either String [JsonValue]
parseArray xs = case partitionEithers $ map parseValue $ divideTopLevel (bracketContent xs '[' ']') ',' of
                ([], x)   -> Right x
                (x:xs, _) -> Left x

-- トップレベルにある文字で文字列を分割する
divideTopLevel :: String -> Char -> [String]
divideTopLevel xs c = divideTopLevelWork xs c []

divideTopLevelWork :: String -> Char -> [Char] -> [String]
divideTopLevelWork "" _ _                         = [""]
divideTopLevelWork ('{':xs) c stack               = let contents = divideTopLevelWork xs c ('}':stack)  -- オブジェクトの開始文字が見つかったので、スタックに終了文字を追加
                                                    in  ('{':(head contents)):(tail contents)
divideTopLevelWork ('[':xs) c stack               = let contents = divideTopLevelWork xs c (']':stack)  -- 配列の開始文字が見つかったので、スタックに終了文字を追加
                                                    in  ('[':(head contents)):(tail contents)
divideTopLevelWork (x:xs) c []        | x == c    = "":(divideTopLevelWork xs c [])                     -- 目的の文字が見つかった
                                      | otherwise = let contents = divideTopLevelWork xs c []
                                                    in  (x:(head contents)):(tail contents)
divideTopLevelWork (x:xs) c (s:stack) | x == s    = let contents = divideTopLevelWork xs c stack        -- スタックにある文字が見つかったので、スタックから削除
                                                    in  (x:(head contents)):(tail contents)
                                      | otherwise = let contents = divideTopLevelWork xs c (s:stack)
                                                    in  (x:(head contents)):(tail contents)

-- JSONテキストから不要なスペースを削除する
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
 - EitherがLeftならLeftを返し、Rightなら関数を適用して返す
 -}
takeLeft :: (b -> Either a c) -> Either a b -> Either a c
takeLeft _ (Left x)   = Left x
takeLeft f (Right x)  = f x

-- JsonObjectをJsonValueに変関する
otov :: JsonObject -> JsonValue
otov x = JsonObject x

-- JsonArrayをJsonValueに変関する
atov :: JsonArray -> JsonValue
atov x = JsonArray x

module Json (JsonValue, JsonObject, parseJson) where 

-- JSONデータ
type JsonPair = (String, JsonValue)
type JsonObject = [JsonPair]
data JsonValue = JsonBool Bool | JsonNumber Float | JsonString String | JsonObject JsonObject | JsonArray [JsonValue] | JsonNull deriving (Show)

-- JSONテキストをパースする
parseJson :: String -> JsonObject
parseJson text = parseObject $ removeWhiteSpace text

-- 囲まれている要素を取り出す
bracketContent :: String -> Char -> Char -> String
bracketContent text sb eb | sb == eb  = sameBracketContent text sb
                          | otherwise = bracketContentLevel text sb eb []

-- 囲まれている階層構造の要素を取り出す
bracketContentLevel :: String -> Char -> Char -> [Char] -> String
bracketContentLevel "" _ _ _                            = ""
bracketContentLevel (x:xs) sb eb []         | x == sb   = bracketContentLevel xs sb eb (eb:[])
                                            | otherwise = ""
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
parsePair :: String -> JsonPair
parsePair text  = let (keyText, ':':valueText) = break (== ':') text
                  in  (bracketContent keyText '"' '"', parseValue valueText)

-- 値をパースする
parseValue :: String -> JsonValue
parseValue text@('{':xs)  = JsonObject $ parseObject text
parseValue text@('[':xs)  = JsonArray $ parseArray text
parseValue text@('"':xs)  = JsonString $ bracketContent text '"' '"'
parseValue "True"         = JsonBool True
parseValue "False"        = JsonBool False
parseValue "null"         = JsonNull
parseValue ""             = JsonNull
parseValue text           = JsonNumber $ read text

-- JSONのオブジェクトをパースする
parseObject :: String -> JsonObject
parseObject xs  = parseObjectContents $ bracketContent xs '{' '}'

-- JSONのオブジェクトの要素をパースする
parseObjectContents :: String -> JsonObject
parseObjectContents xs  = map parsePair $ divideTopLevel xs ','

-- JSONの配列をパースする
parseArray :: String -> [JsonValue]
parseArray xs = map parseValue $ divideTopLevel (bracketContent xs '[' ']') ','

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

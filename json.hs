module Json (JsonObject, JsonValue, parseJson) where 

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
                          | otherwise = bracketContentLevel text sb eb 0

-- 囲まれている階層構造の要素を取り出す
bracketContentLevel :: String -> Char -> Char -> Int -> String
bracketContentLevel "" _ _ _                        = ""
bracketContentLevel (x:xs) sb eb 0      | x == sb   = (bracketContentLevel xs sb eb 1)
                                        | otherwise = bracketContentLevel xs sb eb 0
bracketContentLevel (x:xs) sb eb 1      | x == sb   = x:(bracketContentLevel xs sb eb 2)
                                        | x == eb   = ""
                                        | otherwise = x:(bracketContentLevel xs sb eb 1)
bracketContentLevel (x:xs) sb eb level  | x == sb   = x:(bracketContentLevel xs sb eb (level + 1))
                                        | x == eb   = x:(bracketContentLevel xs sb eb (level - 1))
                                        | otherwise = x:(bracketContentLevel xs sb eb level)

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

-- トップレベルにある値で文字列を分割する
divideTopLevel :: String -> Char -> [String]
divideTopLevel xs c = divideFromIndexes xs $ findTopLevelIndexes xs c 0 0 0

divideFromIndexes :: String -> [(Int, Int)] -> [String]
divideFromIndexes _ []            = []
divideFromIndexes text ((s,e):xs) = (drop s (take e text)):(divideFromIndexes text xs)

findTopLevelIndexes :: String -> Char -> Int -> Int -> Int -> [(Int, Int)]
findTopLevelIndexes "" _ _ index lastIndex                                        = (lastIndex, index):[]
findTopLevelIndexes text@('"':xs) c level index lastIndex                         = let (jsonString, other) = divideJsonString text
                                                                                    in  findTopLevelIndexes other c level (index + length jsonString) lastIndex
findTopLevelIndexes (x:xs) c level index lastIndex        | x == '{'              = findTopLevelIndexes xs c (level + 1) (index + 1) lastIndex
                                                          | x == '}'              = findTopLevelIndexes xs c (level - 1) (index + 1) lastIndex
                                                          | x == '['              = findTopLevelIndexes xs c (level + 1) (index + 1) lastIndex
                                                          | x == ']'              = findTopLevelIndexes xs c (level - 1) (index + 1) lastIndex
                                                          | x == c && level == 0  = (lastIndex, index):(findTopLevelIndexes xs c 0 (index + 1) (index + 1)) -- 目的の文字が見つかった
                                                          | otherwise             = findTopLevelIndexes xs c level (index + 1) lastIndex

dissolveObjectContentStrings :: String -> [String]
dissolveObjectContentStrings ""       = [""]
dissolveObjectContentStrings ('"':xs) = let (jsonString, other) = divideJsonString ('"':xs)
                                        in  let y:ys = dissolveObjectContentStrings other
                                            in (jsonString ++ y):ys
dissolveObjectContentStrings (',':xs) = "":(dissolveObjectContentStrings xs)
dissolveObjectContentStrings (x:xs)   = let y:ys = dissolveObjectContentStrings xs
                                        in (x:y):ys

-- JSONテキストから不要なスペースを削除する
removeWhiteSpace :: String -> String
removeWhiteSpace []         = []
removeWhiteSpace ('"':xs)   = let (jsonString, other) = divideJsonString ('"':xs) -- 文字列の先頭が見つかったのでJSONの文字列の要素は削除しない
                              in jsonString ++ (removeWhiteSpace other)
removeWhiteSpace (' ':xs)   = removeWhiteSpace xs
removeWhiteSpace ('\r':xs)  = removeWhiteSpace xs
removeWhiteSpace ('\n':xs)  = removeWhiteSpace xs
removeWhiteSpace ('\t':xs)  = removeWhiteSpace xs
removeWhiteSpace (x:xs)     = x:removeWhiteSpace xs

-- JSONの文字列を分割する
divideJsonString :: String -> (String, String)
divideJsonString ""       = ("", "")
divideJsonString ('"':xs) = let (as, bs) = divideJsonStringInString xs
                            in ('"':as, bs)
divideJsonString (x:xs)   = let (as, bs) = divideJsonString xs
                            in (x:as, bs)

divideJsonStringInString :: String -> (String, String)
divideJsonStringInString ""           = ("", "")
divideJsonStringInString ('\\':x:xs)  = let (as, bs) = divideJsonStringInString xs
                                        in ('\\':x:as, bs)
divideJsonStringInString ('"':xs)     = ("\"", xs)                                  -- 文字列の終端が見つかったので終了
divideJsonStringInString (x:xs)       = let (as, bs) = divideJsonStringInString xs
                                        in (x:as, bs)

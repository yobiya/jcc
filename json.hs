module Json (JsonObject, JsonValue, parseJSON) where 

import Debug.Trace

-- JSONデータ
type JsonPair = (String, JsonValue)
type JsonObject = [JsonPair]

data JsonValue = JsonValue {
  valueType :: String,
  value     :: String,
  object    :: Maybe JsonObject,
  array     :: [JsonValue]
} deriving (Show)

-- JSONテキストをパースする
parseJSON :: String -> Either String JsonObject
parseJSON "" = Left "JSON text is empty."
parseJSON jsonText = parseObject $ removeWhiteSpace jsonText

-- JSONのオブジェクトをパースする
parseObject :: String -> Either String JsonObject
parseObject xs  = let content = bracketContent xs '{' '}'
                  in  Right $ parseObjectContents content

-- 囲まれている要素を取り出す
bracketContent :: String -> Char -> Char -> String
bracketContent text sb eb = bracketContentLevel text sb eb 0

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

-- キーと値のペアをパースする
-- @todo 適切な実装を行う
parsePair :: String -> JsonPair
parsePair text = (text, JsonValue { valueType="", value="", object=Nothing, array=[] })

-- JSONのオブジェクトの要素をパースする
parseObjectContents :: String -> JsonObject
parseObjectContents xs  = map parsePair $ divideTopLevel xs ','

-- トップレベルにある値で文字列を分割する
divideTopLevel :: String -> Char -> [String]
divideTopLevel xs c = divideFromIndexes xs $ findTopLevelIndexes xs c 0 0 0 False

divideFromIndexes :: String -> [(Int, Int)] -> [String]
divideFromIndexes _ []            = []
divideFromIndexes text ((s,e):xs) = (drop s (take e text)):(divideFromIndexes text xs)

findTopLevelIndexes :: String -> Char -> Int -> Int -> Int -> Bool -> [(Int, Int)]
findTopLevelIndexes "" _ _ index lastIndex _                                          = (lastIndex, index):[]
findTopLevelIndexes ('"':xs) c level index lastIndex False                            = findTopLevelIndexes xs c level (index + 1) lastIndex True                     -- 文字列の先頭が見つかった
findTopLevelIndexes ('\\':x:xs) c level index lastIndex True                          = findTopLevelIndexes xs c level (index + 2) lastIndex True                     -- 文字列中のエスケープ
findTopLevelIndexes ('"':xs) c level index lastIndex True                             = findTopLevelIndexes xs c level (index + 1) lastIndex False                    -- 文字列の終端が見つかった
findTopLevelIndexes (x:xs) c level index lastIndex True                               = findTopLevelIndexes xs c level (index + 1) lastIndex True
findTopLevelIndexes (x:xs) c level index lastIndex False      | x == '{'              = findTopLevelIndexes xs c (level + 1) (index + 1) lastIndex False
                                                              | x == '}'              = findTopLevelIndexes xs c (level - 1) (index + 1) lastIndex False
                                                              | x == '['              = findTopLevelIndexes xs c (level + 1) (index + 1) lastIndex False
                                                              | x == ']'              = findTopLevelIndexes xs c (level - 1) (index + 1) lastIndex False
                                                              | x == c && level == 0  = (lastIndex, index):(findTopLevelIndexes xs c 0 (index + 1) (index + 2) False) -- 目的の文字が見つかった
                                                              | otherwise             = findTopLevelIndexes xs c level (index + 1) lastIndex False

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

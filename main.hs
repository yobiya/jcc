-- JSON Constitution Checker

import System.Environment
import Control.Exception

main = do
  args <- getArgs
  let c:constitutionFileName:t:targetFileName:_ = args
  constitution <- catch (readFile constitutionFileName) (readErrorHander constitutionFileName)
  target <- catch (readFile targetFileName) (readErrorHander targetFileName)

  putStrLn constitution
  putStrLn "----------"
  print $ parseJSON constitution
  putStrLn "----------"
  putStrLn target

readErrorHander :: String -> IOError -> IO String
readErrorHander fileName error = do
  putStrLn $ "Can not open File \"" ++ fileName ++ "\""
  return ""

-- JSONデータ
type JsonObject = [(String, JsonValue)]

data JsonValue = JsonValue {
  valueType :: String,
  value     :: String,
  object    :: Maybe JsonObject,
  array     :: [JsonValue]
} deriving (Show)

-- JSONテキストをパースする
parseJSON :: String -> Either String JsonObject
parseJSON [] = Left "Empty JSON text"
parseJSON jsonText = parseJsonObject $ removeJsonWhiteSpace jsonText

-- JSONのオブジェクトをパースする
parseJsonObject :: String -> Either String JsonObject
parseJsonObject xs  | head xs == '{' && last xs == '}'  = Right $ parseJsonObjectContents $ (tail . init) xs
                    | otherwise                         = Left "Not func JSON object {} pair."

-- JSONのオブジェクトの要素をパースする
parseJsonObjectContents :: String -> JsonObject
parseJsonObjectContents text = [("a", JsonValue { valueType="dummy", value=head $ dissolveJsonObjectContentStrings text, object=Nothing, array=[]})]

dissolveJsonObjectContentStrings :: String -> [String]
dissolveJsonObjectContentStrings ""             = []
dissolveJsonObjectContentStrings text@('"':xs)  = let index = findIndexWithoutJsonString text ','
                                                  in (take index text):(dissolveJsonObjectContentStrings $ drop index text)

findIndexWithoutJsonString :: String -> Char -> Int
findIndexWithoutJsonString xs c = findIndexWithoutJsonString' xs c 0 False

findIndexWithoutJsonString' :: String -> Char -> Int -> Bool -> Int
findIndexWithoutJsonString' ('"':xs) c index False = findIndexWithoutJsonString' xs c (index + 1) True
findIndexWithoutJsonString' (x:xs) c index False = if x == c then index else findIndexWithoutJsonString' xs c (index + 1) False
findIndexWithoutJsonString' ('"':xs) c index True = findIndexWithoutJsonString' xs c (index + 1) False
findIndexWithoutJsonString' (x:xs) c index True = findIndexWithoutJsonString' xs c (index + 1) True


-- JSONテキストから不要なスペースを削除する
removeJsonWhiteSpace :: String -> String
removeJsonWhiteSpace []         = []
removeJsonWhiteSpace ('"':xs)   = '"':skipJsonString xs removeJsonWhiteSpace  -- 文字列の先頭が見つかったのでJSONの文字列の処理を行う
removeJsonWhiteSpace (' ':xs)   = removeJsonWhiteSpace xs
removeJsonWhiteSpace ('\r':xs)  = removeJsonWhiteSpace xs
removeJsonWhiteSpace ('\n':xs)  = removeJsonWhiteSpace xs
removeJsonWhiteSpace ('\t':xs)  = removeJsonWhiteSpace xs
removeJsonWhiteSpace (x:xs)     = x:removeJsonWhiteSpace xs

-- JSONの文字列をスキップする
skipJsonString :: String -> (String -> String) -> String
skipJsonString [] _             = []
skipJsonString ('\\':x:xs) end  = '\\':x:skipJsonString xs end  -- 文字列内のエスケープ処理
skipJsonString ('"':xs) end     = '"':end xs                    -- 文字列の終端に達した
skipJsonString (x:xs) end       = x:skipJsonString xs end

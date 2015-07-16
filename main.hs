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

parseJsonObjectContents :: String -> JsonObject
parseJsonObjectContents _ = [("a", JsonValue { valueType="dummy", value="", object=Nothing, array=[]})]

-- JSONテキストから不要なスペースを削除する
removeJsonWhiteSpace :: String -> String
removeJsonWhiteSpace []         = []
removeJsonWhiteSpace ('"':xs)   = '"':skipJsonString xs -- 文字列の先頭が見つかったのでJSONの文字列の処理を行う
removeJsonWhiteSpace (' ':xs)   = removeJsonWhiteSpace xs
removeJsonWhiteSpace ('\r':xs)  = removeJsonWhiteSpace xs
removeJsonWhiteSpace ('\n':xs)  = removeJsonWhiteSpace xs
removeJsonWhiteSpace ('\t':xs)  = removeJsonWhiteSpace xs
removeJsonWhiteSpace (x:xs)     = x:removeJsonWhiteSpace xs

-- JSONの文字列をスキップする
skipJsonString :: String -> String
skipJsonString []           = []
skipJsonString ('\\':x:xs)  = '\\':x:skipJsonString xs    -- 文字列内のエスケープ処理
skipJsonString ('"':xs)     = '"':removeJsonWhiteSpace xs -- 文字列の終端に達した
skipJsonString (x:xs)       = x:skipJsonString xs

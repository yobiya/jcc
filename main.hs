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
dissolveJsonObjectContentStrings ""       = [""]
dissolveJsonObjectContentStrings ('"':xs) = let (jsonString, other) = divideJsonString ('"':xs)
                                            in  let y:ys = dissolveJsonObjectContentStrings other
                                                in (jsonString ++ y):ys
dissolveJsonObjectContentStrings (',':xs)   = "":(dissolveJsonObjectContentStrings xs)
dissolveJsonObjectContentStrings (x:xs)     = let y:ys = dissolveJsonObjectContentStrings xs
                                              in (x:y):ys

-- JSONテキストから不要なスペースを削除する
removeJsonWhiteSpace :: String -> String
removeJsonWhiteSpace []         = []
removeJsonWhiteSpace ('"':xs)   = let (jsonString, other) = divideJsonString ('"':xs) -- 文字列の先頭が見つかったのでJSONの文字列の要素は削除しない
                                  in jsonString ++ (removeJsonWhiteSpace other)
removeJsonWhiteSpace (' ':xs)   = removeJsonWhiteSpace xs
removeJsonWhiteSpace ('\r':xs)  = removeJsonWhiteSpace xs
removeJsonWhiteSpace ('\n':xs)  = removeJsonWhiteSpace xs
removeJsonWhiteSpace ('\t':xs)  = removeJsonWhiteSpace xs
removeJsonWhiteSpace (x:xs)     = x:removeJsonWhiteSpace xs

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

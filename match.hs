module Match (matchConstitution) where 

import Data.Data
import Data.Maybe
import Data.List
import Json

{-
 - JSONの構成が条件に合っているか判定する
 -
 - JsonObject 判定の対象となるJSONの値
 - JsonObject 構成情報を持つJSONのオブジェクト
 - Bool       条件に合っていればTrue
 -}
matchConstitution :: JsonValue -> JsonObject -> Bool
matchConstitution t c = matchTypeFromName t (flattenTypes c) "main"

{-
 - 構成条件の型をフラットなリストにする
 -
 - JsonObject 構成条件を持つJsonObject
 - [JsonPair] 構成条件のキーと値のペアリスト
 -}
flattenTypes :: JsonObject -> [JsonPair]
flattenTypes c  = (maybeToList $ find (\pair -> "main" == fst pair) c) ++ (maybe [] jsonObjectContents $ lookup "type" c)

{-
 - 指定した名前の型に一致した構成になっているか判定する
 -
 - JsonValue  判定される値
 - [JsonPair] 型名と構成情報のリスト
 - String     構成情報の型名
 - Bool       条件に合っていればTrue
 -}
matchTypeFromName :: JsonValue -> [JsonPair] -> String -> Bool
matchTypeFromName t types typeName  = matchValueType types (Just t) $ fromMaybe JsonNull $ lookup typeName types

{-
 - 値が構成に一致しているか判定する
 -
 - [JsonPair]       構成条件のキーと値のペア
 - Maybe JsonValue  判定される値
 - JsonValue        構成情報の値
 - Bool             一致している場合はTrue
 -}
matchValueType :: [JsonPair] -> Maybe JsonValue -> JsonValue -> Bool
matchValueType types t (JsonArray c)                          = any id $ map (matchValueType types t) c
matchValueType types (Just (JsonObject t)) (JsonObject c)     = all id $ map (\(key, value) -> matchValueType types (lookup key t) value) c
matchValueType _ (Just (JsonBool t)) (JsonString "bool")      = True
matchValueType _ (Just (JsonNumber t)) (JsonString "number")  = True
matchValueType _ (Just (JsonString t)) (JsonString "string")  = True
matchValueType _ (Just JsonNull) (JsonString "null")          = True
matchValueType _ Nothing (JsonString "none")                  = True
matchValueType types (Just t) (JsonString c)                  = matchTypeFromName t types c
matchValueType _ _ _                                          = False


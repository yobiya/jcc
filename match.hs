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
matchTypeFromName t types typeName  = matchType types (Just t) $ fromMaybe JsonNull $ lookup typeName types

{-
 - 値が構成に一致しているか判定する
 -
 - [JsonPair]       構成条件のキーと値のペア
 - Maybe JsonValue  判定される値
 - JsonValue        構成情報の値
 - Bool             一致している場合はTrue
 -}
matchType :: [JsonPair] -> Maybe JsonValue -> JsonValue -> Bool
matchType types t (JsonArray c)                       = any id $ map (matchType types t) c
matchType types (Just (JsonObject t)) (JsonObject c)  = all id $ map (\(key, value) -> matchType types (lookup key t) value) c
matchType types t (JsonString c)                      = matchTypeFromString types t $ filter (/= ' ') c
matchType _ _ _                                       = False

{-
 - 値が文字列で表される構成に一致しているか判定する
 -
 - [JsonPair]       構成条件のキーと値のペア
 - Maybe JsonValue  判定される値
 - String           構成情報の文字列
 - Bool             一致している場合はTrue
 -}
matchTypeFromString :: [JsonPair] -> Maybe JsonValue -> String -> Bool
matchTypeFromString _ (Just t) "any"                  = True  -- 要素があれば良い
matchTypeFromString _ (Just (JsonBool t)) "bool"      = True
matchTypeFromString _ (Just (JsonNumber t)) "number"  = True
matchTypeFromString _ (Just (JsonString t)) "string"  = True
matchTypeFromString _ (Just JsonNull) "null"          = True
matchTypeFromString _ Nothing "none"                  = True  -- 要素が無ければ良い
matchTypeFromString types (Just t) c                  = matchTypeFromName t types c
matchTypeFromString _ _ _                             = False

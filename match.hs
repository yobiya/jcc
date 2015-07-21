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
matchTypeFromName t types typeName  = maybe False (matchValueType (Just t)) $ lookup typeName types

{-
 - 値が構成に一致しているか判定する
 -
 - Maybe JsonValue  判定される値
 - JsonValue        構成情報の値
 - Bool             一致している場合はTrue
 -}
matchValueType :: Maybe JsonValue -> JsonValue -> Bool
matchValueType t (JsonArray c)                              = any id $ map (matchValueType t) c
matchValueType (Just (JsonObject t)) (JsonObject c)         = matchObjectType t c
matchValueType (Just (JsonBool t)) (JsonString "bool")      = True
matchValueType (Just (JsonNumber t)) (JsonString "number")  = True
matchValueType (Just (JsonString t)) (JsonString "string")  = True
matchValueType (Just JsonNull) (JsonString "null")          = True
matchValueType Nothing (JsonString "none")                  = True
matchValueType _ _                                          = False

{-
 - オブジェクトが構成に一致しているか判定する
 -
 - JsonObject 判定されるオブジェクト
 - JsonObject 構成情報オブジェクト
 - Bool       一致している場合はTrue
 -}
matchObjectType :: JsonObject -> JsonObject -> Bool
matchObjectType t c = all id $ map (matchPair t) c

{-
 - 一致するキーと型を持っているか判定する
 -
 - JsonObject 判定される構造
 - JsonPair   構成情報のキーと型のペア
 - Bool       一致している場合はTrue
 -}
matchPair :: JsonObject -> JsonPair -> Bool
matchPair t (key, value)  = matchValueType (lookup key t) value


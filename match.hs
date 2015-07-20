module Match (matchConstitution) where 

import Data.Maybe
import Data.List
import Json

import Debug.Trace

{-
 - JSONの構成が条件に合っているか判定する
 -
 - JsonObject 構成情報を持つJsonObject
 - JsonObject 判定の対象となるJsonObject
 - Bool       条件に合っていればTrue
 -}
matchConstitution :: JsonObject -> JsonObject -> Bool
matchConstitution c t = let types = flattenTypes c
                        in  matchTypeFromName t types "main"

-- 構成条件の型をフラットなリストにする
flattenTypes :: JsonObject -> [JsonPair]
flattenTypes c = (maybeToList $ find (\pair -> "main" == fst pair) c) ++ (maybe [] jsonObjectContents $ lookup "type" c)

-- 指定した名前の型に一致した構成になっているか判定する
matchTypeFromName :: JsonObject -> [JsonPair] -> String -> Bool
matchTypeFromName t types typeName  = maybe False (matchType t) $ lookup typeName types

{-
 - 型に一致した構成になっているか判定する
 -
 - JsonObject 判定される構造
 - JsonValue  構成情報
 - Bool       一致している場合はTrue
 -}
matchType :: JsonObject -> JsonValue -> Bool
matchType t (JsonObject c)  = all id $ map (\pair -> matchPair pair t) c
matchType _ _               = False

{-
 - 一致するキーと型を持っているか判定する
 -
 - JsonPair   キーと型のペア
 - JsonObject 判定される構造
 - Bool       一致している場合はTrue
 -}
matchPair :: JsonPair -> JsonObject -> Bool
matchPair (key, value) t  = let maybeTValue = lookup key t
                            in  isJust maybeTValue

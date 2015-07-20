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
flattenTypes c  = let maybeMainType = findJsonPair "main" c
                      maybeSubTypes = lookup "type" c
                  in  let mainTypes = maybeToList maybeMainType
                          subTypes = if isJust maybeSubTypes then jsonObjectContents $ fromJust maybeSubTypes else []
                      in mainTypes ++ subTypes

-- 指定した名前の型に一致した構成になっているか判定する
matchTypeFromName :: JsonObject -> [JsonPair] -> String -> Bool
matchTypeFromName t types typeName  = let maybeType = findJsonPair typeName types
                                      in  if isJust maybeType then matchType t $ snd $ fromJust maybeType else False

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
matchPair (key, value) t  = let maybePair = findJsonPair key t
                            in  isJust maybePair

{-
 - 指定したキーを持つJsonPairを検索する
 -
 - String         キー
 - [JsonPair]     検索対象
 - Maybe JsonPair 見つかった場合はJust JsonPair
 -}
findJsonPair :: String -> [JsonPair] -> Maybe JsonPair
findJsonPair key pairs  = find (\pair -> key == fst pair) pairs

module Match (matchConstitution) where 

import Data.Maybe
import Data.List
import Json

-- JSONの構成が条件に合っているか判定する
matchConstitution :: JsonObject -> JsonObject -> Bool
matchConstitution c t = let types = flattenTypes c
                        in  matchTypeFromName t types "main"

-- 構成条件の型をフラットなリストにする
flattenTypes :: JsonObject -> [JsonPair]
flattenTypes c  = let maybeMainType = find (\pair -> "main" == fst pair) c
                      maybeSubTypes = lookup "type" c
                  in  let mainTypes = maybeToList maybeMainType
                          subTypes = if isJust maybeSubTypes then jsonObjectContents $ fromJust maybeSubTypes else []
                      in mainTypes ++ subTypes

-- 指定した名前の型に一致した構成になっているか判定する
matchTypeFromName :: JsonObject -> [JsonPair] -> String -> Bool
matchTypeFromName t types typeName  = let maybeType = find (\pair -> typeName == fst pair) types
                                      in  if isJust maybeType then matchType t $ snd $ fromJust maybeType else False

-- 型に一致した構成になっているか判定する
matchType :: JsonObject -> JsonValue -> Bool
matchType t (JsonObject c)  = True 
matchType _ _               = False


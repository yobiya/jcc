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
-- matchConstitution t c = matchTypeFromName t (flattenTypes c) "main"
matchConstitution t c = let maybeEntryName = lookup "entry" c
                            maybeObject = lookup "object" c
                        in  case all isJust (maybeEntryName:maybeObject:[]) of
                            False ->  False
                            True  ->  let (JsonObject object) = fromJust maybeObject
                                          (JsonString entoryName) = fromJust maybeEntryName
                                      in  matchTypeFromName t object entoryName

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
matchType types t (JsonArray c)                       = any (matchType types t) c
matchType types (Just (JsonObject t)) (JsonObject c)  = all (\(key, value) -> matchType types (lookup key t) value) c
matchType types t (JsonString c)                      = matchTypeWithString types t $ filter (/= ' ') c
matchType _ _ _                                       = False

{-
 - 値が文字列で表される構成に一致しているか判定する
 -
 - [JsonPair]       構成条件のキーと値のペア
 - Maybe JsonValue  判定される値
 - String           構成情報の文字列
 - Bool             一致している場合はTrue
 -}
matchTypeWithString :: [JsonPair] -> Maybe JsonValue -> String -> Bool
matchTypeWithString _ (Just t) "any"                          = True  -- 要素があれば良い
matchTypeWithString _ (Just (JsonBool t)) "bool"              = True
matchTypeWithString _ (Just (JsonNumber t)) "number"          = True
matchTypeWithString _ (Just (JsonString t)) "string"          = True
matchTypeWithString _ (Just JsonNull) "null"                  = True
matchTypeWithString _ Nothing "none"                          = True  -- 要素が無ければ良い
matchTypeWithString types (Just (JsonArray t)) text@('[':xs)  = maybe False (matchArrayTypeWithString types t) $ bracketContent text '[' ']'
matchTypeWithString types (Just t) c                          = matchTypeFromName t types c
matchTypeWithString _ _ _                                     = False

{-
 - 配列の値が構成に一致しているか判定する
 -
 - [JsonPair] 構成条件のキーと値のペア
 - JsonArray  判定される配列の値
 - String     構成情報の文字列
 - Bool       一致している場合はTrue
 -}
matchArrayTypeWithString :: [JsonPair] -> JsonArray -> String -> Bool
matchArrayTypeWithString types t c = all id $ map (\target -> matchTypeWithString types (Just target) c) t

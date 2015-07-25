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
 - String     条件に合っていなければエラーメッセージ
 -}
matchConstitution :: JsonValue -> JsonObject -> String
matchConstitution t c = let maybeEntryName = lookup "entry" c
                            maybeObject = lookup "object" c
                        in  case all isJust (maybeEntryName:maybeObject:[]) of
                            False ->  "error"
                            True  ->  let (JsonObject object) = fromJust maybeObject
                                          (JsonString entoryName) = fromJust maybeEntryName
                                      in  matchTypeFromName t object entoryName

{-
 - 指定した名前の型に一致した構成になっているか判定する
 -
 - JsonValue  判定される値
 - [JsonPair] 型名と構成情報のリスト
 - String     構成情報の型名
 - String     条件に合っていなければエラーメッセージ
 -}
matchTypeFromName :: JsonValue -> [JsonPair] -> String -> String
matchTypeFromName t types typeName  = matchType types (Just t) $ fromMaybe JsonNull $ lookup typeName types

{-
 - 値が構成に一致しているか判定する
 -
 - [JsonPair]       構成条件のキーと値のペア
 - Maybe JsonValue  判定される値
 - JsonValue        構成情報の値
 - String           条件に合っていなければエラーメッセージ
 -}
matchType :: [JsonPair] -> Maybe JsonValue -> JsonValue -> String
matchType types t (JsonArray c)                       = if any (\s -> s == "") $ map (matchType types t) c then "" else "error"
matchType types (Just (JsonObject t)) (JsonObject c)  = if all (\s -> s == "") $ map (\(key, value) -> matchType types (lookup key t) value) c then "" else "error"
matchType types t (JsonString c)                      = matchTypeWithString types t $ filter (/= ' ') c
matchType _ _ _                                       = "error"

{-
 - 値が文字列で表される構成に一致しているか判定する
 -
 - [JsonPair]       構成条件のキーと値のペア
 - Maybe JsonValue  判定される値
 - String           構成情報の文字列
 - String           条件に合っていなければエラーメッセージ
 -}
matchTypeWithString :: [JsonPair] -> Maybe JsonValue -> String -> String
matchTypeWithString _ (Just t) "any"                          = ""  -- 要素があれば良い
matchTypeWithString _ (Just (JsonBool t)) "bool"              = ""
matchTypeWithString _ (Just (JsonNumber t)) "number"          = ""
matchTypeWithString _ (Just (JsonString t)) "string"          = ""
matchTypeWithString _ (Just JsonNull) "null"                  = ""
matchTypeWithString _ Nothing "none"                          = ""  -- 要素が無ければ良い
matchTypeWithString types (Just (JsonArray t)) text@('[':xs)  = maybe "" (matchArrayTypeWithString types t) $ bracketContent text '[' ']'
matchTypeWithString types (Just t) c                          = matchTypeFromName t types c
matchTypeWithString _ _ _                                     = "error"

{-
 - 配列の値が構成に一致しているか判定する
 -
 - [JsonPair] 構成条件のキーと値のペア
 - JsonArray  判定される配列の値
 - String     構成情報の文字列
 - String     条件に合っていなければエラーメッセージ
 -}
matchArrayTypeWithString :: [JsonPair] -> JsonArray -> String -> String
matchArrayTypeWithString types t c = if all (\s -> s == "") $ map (\target -> matchTypeWithString types (Just target) c) t then "" else "error"

module Match (matchConstitution) where 

import Data.Data
import Data.Maybe
import Data.List
import Json
import Message

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
matchType types t (JsonArray c)                       = case partition isMatchMessage $ map (matchType types t) c of
                                                        ([], e:es)  -> e      -- 配列の中のどれともマッチしなかった
                                                        _           -> mMatch -- 配列の中のどれかにマッチした
matchType types (Just (JsonObject t)) (JsonObject c)  = case filter (not . isMatchMessage) $ map (\(key, value) -> matchType types (lookup key t) value) c of
                                                        []    -> mMatch
                                                        e:es  -> e
matchType types t (JsonString c)                      = matchTypeWithString types t $ filter (/= ' ') c
matchType _ Nothing _                                 = "error"
matchType _ (Just t) c                                = "Not found " ++ valueToText t ++ " match pattern."

{-
 - 値が文字列で表される構成に一致しているか判定する
 -
 - [JsonPair]       構成条件のキーと値のペア
 - Maybe JsonValue  判定される値
 - String           構成情報の文字列
 - String           条件に合っていなければエラーメッセージ
 -}
matchTypeWithString :: [JsonPair] -> Maybe JsonValue -> String -> String
matchTypeWithString _ (Just t) "any"                          = mMatch  -- 要素があれば良い
matchTypeWithString _ (Just (JsonBool t)) "bool"              = mMatch
matchTypeWithString _ (Just (JsonNumber t)) "number"          = mMatch
matchTypeWithString _ (Just (JsonString t)) "string"          = mMatch
matchTypeWithString _ (Just JsonNull) "null"                  = mMatch
matchTypeWithString _ Nothing "none"                          = mMatch  -- 要素が無ければ良い
matchTypeWithString types (Just (JsonArray t)) text@('[':xs)  = maybe mMatch (matchArrayTypeWithString types t) $ bracketContent text '[' ']'
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
matchArrayTypeWithString types t c  = case partition isMatchMessage $ map (\target -> matchTypeWithString types (Just target) c) t of
                                      (_, e:es) -> (valueToText $ (\x -> (JsonArray x)) t) ++ " -- " ++ e      -- 配列の中のどれかがマッチしなかった
                                      _         -> mMatch -- 配列の中の全てがマッチした

{-
 - 一致しているメッセージか判定する
 -
 - String メッセージ
 - Bool   一致している場合はTrue
 -}
isMatchMessage :: String -> Bool
isMatchMessage s = s == mMatch


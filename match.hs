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
                                      in  matchTypeFromName ("", t) object entoryName -- ルートオブジェクトに名前はない

{-
 - 指定した名前の型に一致した構成になっているか判定する
 -
 - JsonPair   キーと判定される値のペア
 - [JsonPair] 型名と構成情報のリスト
 - String     構成情報の型名
 - String     条件に合っていなければエラーメッセージ
 -}
matchTypeFromName :: JsonPair -> [JsonPair] -> String -> String
matchTypeFromName (key, t) types typeName  = matchType types (Just t) $ (key, fromMaybe JsonNull $ lookup typeName types)

{-
 - 値が構成に一致しているか判定する
 -
 - [JsonPair]       構成条件のキーと値のペア
 - Maybe JsonValue  判定される値
 - JsonPair         キーと構成情報の値のペア
 - String           条件に合っていなければエラーメッセージ
 -}
matchType :: [JsonPair] -> Maybe JsonValue -> JsonPair -> String
matchType types t (key, JsonArray c)                    = case partition isMatchMessage $ map (\v -> matchType types t (key, v)) c of
                                                          ([], e:es)  -> e      -- 配列の中のどれともマッチしなかった
                                                          _           -> mMatch -- 配列の中のどれかにマッチした
matchType types (Just (JsonObject t)) (_, JsonObject c) = case filter (not . isMatchMessage) $ map (\(key, value) -> matchType types (lookup key t) (key, value)) c of
                                                          []    -> mMatch
                                                          e:es  -> e
matchType types t (key, (JsonString c))                 = matchTypeWithString types t $ (key, filter (/= ' ') c)
matchType _ Nothing _                                   = "error"
matchType _ (Just t) c                                  = emNotFoundJsonPattern $ valueToText t

{-
 - 値が文字列で表される構成に一致しているか判定する
 -
 - [JsonPair]       構成条件のキーと値のペア
 - Maybe JsonValue  判定される値
 - (String, String) キーとその構成情報の文字列のペア
 - String           条件に合っていなければエラーメッセージ
 -}
matchTypeWithString :: [JsonPair] -> Maybe JsonValue -> (String, String) -> String
matchTypeWithString _ (Just t) (_, "any")                           = mMatch  -- 要素があれば良い
matchTypeWithString _ (Just (JsonBool t)) (_, "bool")               = mMatch
matchTypeWithString _ (Just (JsonNumber t)) (_, "number")           = mMatch
matchTypeWithString _ (Just (JsonString t)) (_, "string")           = mMatch
matchTypeWithString _ (Just JsonNull) (_, "null")                   = mMatch
matchTypeWithString _ Nothing (_, "none")                           = mMatch  -- 要素が無ければ良い
matchTypeWithString types (Just (JsonArray t)) (key, text@('[':xs)) = either (\_ -> mMatch) (matchArrayTypeWithString types (key, t)) $ bracketContent text '[' ']'
matchTypeWithString types (Just t) (key, c)                         = matchTypeFromName (key, t) types c
matchTypeWithString _ _ (key, _)                                    = emNotFoundKey key

{-
 - 配列の値が構成に一致しているか判定する
 -
 - [JsonPair]           構成条件のキーと値のペア
 - (String, JsonArray)  キーと判定される配列の値のペア
 - String               構成情報の文字列
 - String               条件に合っていなければエラーメッセージ
 -}
matchArrayTypeWithString :: [JsonPair] -> (String, JsonArray) -> String -> String
matchArrayTypeWithString types (key, t) c = case partition isMatchMessage $ map (\target -> matchTypeWithString types (Just target) (key, c)) t of
                                            (_, e:es) -> (valueToText $ JsonArray t) ++ " -- " ++ e   -- 配列の中のどれかがマッチしなかった
                                            _         -> mMatch                                       -- 配列の中の全てがマッチした

{-
 - 一致しているメッセージか判定する
 -
 - String メッセージ
 - Bool   一致している場合はTrue
 -}
isMatchMessage :: String -> Bool
isMatchMessage s = s == mMatch


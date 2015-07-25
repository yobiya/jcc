-- JSON Constitution Checker

import System.Environment
import Control.Exception
import Data.Either
import Data.List
import Json
import Match
import Message

main = do
  c:constitutionFileName:t:targetFileName:_ <- getArgs
  constitution <- catch (readFile constitutionFileName) (readErrorHander constitutionFileName)
  target <- catch (readFile targetFileName) (readErrorHander targetFileName)

  putStrLn $ match $ zip (constitutionFileName:targetFileName:[]) $ map parseJson (constitution:target:[])

{-
 - 構成が正しいか判定する
 -
 - [(String, Either String JsonObject)] ファイル名とJSONのパース結果のタプルリスト
 -}
match :: [(String, Either String JsonObject)] -> String
match xs  = case find (\(n, e) -> isLeft e) xs of
            Just (n, e) ->  emParseError n e
            Nothing     ->  let (_, Right c) = xs!!0
                                (tn, Right t) = xs!!1
                            in  case matchConstitution ((\x -> (JsonObject x)) t) c of
                                ""  -> tn ++ " is match."
                                e   -> tn ++ " is no tmatch : " ++ e

-- ファイル読み込みエラー処理
readErrorHander :: String -> IOError -> IO String
readErrorHander fileName error = do
  putStrLn $ "Can not open File \"" ++ fileName ++ "\""
  return ""


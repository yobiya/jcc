-- JSON Constitution Checker

import System.Environment
import Control.Exception
import Data.Either
import Json
import Match

main = do
  args <- getArgs
  let c:constitutionFileName:t:targetFileName:_ = args
  constitution <- catch (readFile constitutionFileName) (readErrorHander constitutionFileName)
  target <- catch (readFile targetFileName) (readErrorHander targetFileName)

  let jsons = map parseJson (constitution:target:[])
  putStrLn $ message (partitionEithers jsons) targetFileName

message :: ([String], [JsonObject]) -> String -> String
message ([], xs) targetFileName = let isMatch = matchConstitution ((\x -> (JsonObject x)) $ xs!!1) (xs!!0)
                                  in  if isMatch then targetFileName ++ " is match." else targetFileName ++ " is not match."

-- ファイル読み込みエラー処理
readErrorHander :: String -> IOError -> IO String
readErrorHander fileName error = do
  putStrLn $ "Can not open File \"" ++ fileName ++ "\""
  return ""


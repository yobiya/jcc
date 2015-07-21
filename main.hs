-- JSON Constitution Checker

import System.Environment
import Control.Exception
import Json
import Match

main = do
  args <- getArgs
  let c:constitutionFileName:t:targetFileName:_ = args
  constitution <- catch (readFile constitutionFileName) (readErrorHander constitutionFileName)
  target <- catch (readFile targetFileName) (readErrorHander targetFileName)

  let isMatch = matchConstitution (JsonObject (parseJson target)) (parseJson constitution)
  putStrLn (if isMatch then targetFileName ++ " is match." else targetFileName ++ " is not match.")


-- ファイル読み込みエラー処理
readErrorHander :: String -> IOError -> IO String
readErrorHander fileName error = do
  putStrLn $ "Can not open File \"" ++ fileName ++ "\""
  return ""


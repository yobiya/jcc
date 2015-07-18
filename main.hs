-- JSON Constitution Checker

import System.Environment
import Control.Exception
import Json

main = do
  args <- getArgs
  let c:constitutionFileName:t:targetFileName:_ = args
  constitution <- catch (readFile constitutionFileName) (readErrorHander constitutionFileName)
  target <- catch (readFile targetFileName) (readErrorHander targetFileName)

  putStrLn constitution
  putStrLn "----------"
  print $ parseJson constitution
  putStrLn "----------"
  putStrLn target


-- ファイル読み込みエラー処理
readErrorHander :: String -> IOError -> IO String
readErrorHander fileName error = do
  putStrLn $ "Can not open File \"" ++ fileName ++ "\""
  return ""


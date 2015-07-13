-- JSON Constitution Checker

import System.Environment
import Control.Exception

main = do
  args <- getArgs
  let fileName = (head args)
  fileContents <- catch (readFile fileName) (readErrorHander fileName)
  putStr fileContents;

readErrorHander :: String -> IOError -> IO String
readErrorHander fileName error = do
  putStrLn $ "Can not open File \"" ++ fileName ++ "\""
  return []

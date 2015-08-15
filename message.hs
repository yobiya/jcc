module Message where 

import Data.Either

-- 型
type Fragile = Either String  -- 要素かエラーメッセージを持つ

-- 成功メッセージ
mMatch  = ""

-- エラーメッセージ
emlNonBracketPair sb eb     = Left $ "Not found bracket pair " ++ sb:' ':eb:'.':[]
emlNonObjectKeyValuePair x  = Left $ "Not found object content key value pair. (" ++ x ++ ")"
emlNonValue                 = Left $ "Not found value."
emlNonCloseBracket c        = Left $ "Not found close bracket. (" ++ c:")"
emParseError n e            = "File \"" ++ n ++ "\" parse error : " ++ e
emNotFoundKey key           = "Not found \"" ++ key ++ "\" key."
emNotFoundJsonPattern v     = "Not found " ++ v ++ " match pattern."
emCanNotOpenFile n          = "Can not open File \"" ++ n ++ "\""

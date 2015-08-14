module Message where 

import Data.Either

-- 型
type Fragile = Either String  -- 要素かエラーメッセージを持つ

-- 成功メッセージ
mMatch  = ""

-- エラーメッセージ
emNonBracketPair sb eb    = Left $ "Not found bracket pair " ++ sb:' ':eb:'.':[]
emNonObjectKeyValuePair x = Left $ "Not found object content key value pair. (" ++ x ++ ")"
emNonValue                = Left $ "Not found value."
emNonCloseBracket c       = Left $ "Not found close bracket. (" ++ c:")"
emParseError n e          = "File \"" ++ n ++ "\" parse error : " ++ e
emNotFoundKey key         = "Not found \"" ++ key ++ "\" key."
emNotFoundJsonPattern v   = "Not found " ++ v ++ " match pattern."
emCanNotOpenFile n        = "Can not open File \"" ++ n ++ "\""

module Message where 

-- 成功メッセージ
mMatch  = ""

-- エラーメッセージ
emNonBracketPair sb eb    = Left $ "Not found bracket pair " ++ sb:' ':eb:'.':[]
emNonObjectKeyValuePair x = Left $ "Not found object content key value pair. (" ++ x ++ ")"
emNonValue                = Left $ "Not found value."
emNonCloseBracket c       = Left $ "Not found close bracket. (" ++ c:")"
emParseError n (Left e)   = "File \"" ++ n ++ "\" parse error : " ++ e
emNotFoundKey key         = "Not found " ++ key ++ " key."
emNotFoundJsonPattern v   = "Not found " ++ v ++ " match pattern."

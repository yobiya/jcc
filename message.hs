module Message where 

-- エラーメッセージ
emNonBracketPair sb eb    = Left $ "Not found bracket pair " ++ sb:' ':eb:'.':[]
emNonObjectKeyValuePair x = Left $ "Not found object content key value pair. (" ++ x ++ ")"
emNonValue                = Left $ "Not found value."
emNonCloseBracket c       = Left $ "Not found close bracket. (" ++ c:")"

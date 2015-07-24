module Message where 

-- エラーメッセージ
emNonBracketPair sb eb = Left $ "Not found bracket pair " ++ sb:' ':eb:'.':[]

module Match (matchConstitution) where 

import Data.Maybe
import Json

-- JSONの構成が条件に合っているか判定する
matchConstitution :: JsonObject -> JsonObject -> Bool
matchConstitution c t = let mainType = lookup "main" c
                        in  if isJust mainType then matchType (fromJust mainType) t else False

matchType :: JsonValue -> JsonObject -> Bool
matchType _ _ = False

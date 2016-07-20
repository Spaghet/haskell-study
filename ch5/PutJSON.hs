module PutJSON
  (
    renderJValue
  ) where

import SimpleJSON
import Data.List (intercalate)

renderJValue :: JValue -> String
renderJValue  (JString s)   = show s
renderJValue  (JNumber n)   = show n
renderJValue  (JBool True)  = "true"
renderJValue  (JBool False) = "false"
renderJValue  (JNull)       = "null"

renderJValue  (JObject o)   = "{" ++ pairs o ++ "}"
  where
    pairs [] = ""
    pairs ps = intercalate ", " (map renderPair ps)
    renderPair (k,v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray ar)    = "[" ++ values ar ++ "]"
  where
    values [] = ""
    values a  = intercalate ", " (map renderJValue a)

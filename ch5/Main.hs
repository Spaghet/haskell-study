module Main (main) where

import SimpleJSON
import PutJSON

main =
  print (renderJValue jVal)
  where
    jVal = JObject ([("Key", JString "Value"), ("k", JNumber 2)])

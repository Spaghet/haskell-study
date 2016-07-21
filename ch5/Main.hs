module Main (main) where

import SimpleJSON
import PrettyJSON
import Prettify

main :: IO ()
main =
  putStrLn (pretty 80 . renderJValue $ jVal)
  where
    jVal = JObject ([("Key", JString "Value"), ("k", JNumber 2)])

import Data.Char (digitToInt)

asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc (x:xs) =
  loop ((acc * 10) + (digitToInt x)) xs
loop acc [] = acc

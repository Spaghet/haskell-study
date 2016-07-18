import Data.List (foldl')
import Data.Char (digitToInt, isDigit)
import Debug.Trace (trace)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred [] =
  []
splitWith pred xs =
  let
    (fs, sc) = break (not . pred) xs
  in
    fs : splitWith pred (safeTail sc)


safeTail :: [a] -> [a]
safeTail (_:xs) = xs
safeTail _ = []

type ErrorMessage = String

asInt :: String -> Either Int ErrorMessage
asInt [] = Right "No input"
asInt "-" = Right "No number found"
asInt ('-':s) =  case acc of
  Left x -> Left ((-1) * x)
  Right x -> acc
  where
    acc = asInt s
asInt s =   (foldl' step (Left 0) s)
  where
    step (Right errorMessage) _ = Right errorMessage
    step (Left acc) head
      | isDigit head = Left ((acc * 10) + (digitToInt head))
      | otherwise = Right "non-digit"

myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

takeWhileExplicit :: (a -> Bool) -> [a] -> [a]
takeWhileExplicit pred xs = reverse $ loop [] pred xs
  where
    loop acc pred (x:xs) =
      if pred x
      then loop (x:acc) pred xs
      else loop acc pred []
    loop acc _ [] = acc

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred xs = foldr step [] xs
  where
    step x acc
      | pred x = x:acc
      | otherwise = []

import Data.List (tails, intersect)
import Data.Char (isUpper)

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

suffixes2 xs = init (tails xs)

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

suffixes3 xs = compose init tails xs

suffixes4 = compose init tails
suffixes5 = init . tails

countUpper :: String -> Int
countUpper = length . filter (isUpper . head) . words

triangleSquare :: Int -> [Int]
triangleSquare n = squares `intersect` triangles
  where
    squares = map (^2) [1..n]
    triangles = map triangleFormula [1..n]
    triangleFormula m = truncate ((toRational (m*(m+1))) / 2) :: Int

import Data.List (sortBy)

add a b = a + b

myDrop n ls = if n <= 0 || null ls
  then ls
  else myDrop (n - 1) (tail ls)

myTake n ls = if n <= 0 || null ls
  then ls
  else myTake (n - 1) (init ls)

lastButOne xs =
  last . init $ xs

myNot True = False
myNot False = True

mySum (x:xs) = x + mySum xs
mySum [] = 0

mySum2 xs = if null xs
  then 0
  else (head xs) + (mySum . tail $ xs)


myLen (x:xs) =
  1 + myLen xs
myLen [] =
  0

myMean :: [Int] -> Float
myMean xs =
  (fromIntegral . sum $ xs) / (fromIntegral . length $ xs)

mirror :: [a] -> [a]
mirror xs =
  xs ++ (reverse xs)


isPalindrome xs
  | length xs == 1 = True
  | xs == [] = True
  | head xs /= last xs = False
  | head xs == last xs = isPalindrome (tail . init $ xs)


sortByLength xs =
  sortBy predicate xs
  where
    predicate a b
      | length a > length b = GT
      | length a == length b = EQ
      | length a < length b = LT

fizzBuzz :: Int -> String
fizzBuzz n = foldr step "" [1..n]
  where step x acc  | x `mod` 15 == 0 = "fizzbuzz " ++ acc
                    | x `mod` 5 == 0  = "buzz " ++ acc
                    | x `mod` 3 == 0  = "fizz " ++ acc
                    | otherwise = (show x) ++ ' ':acc

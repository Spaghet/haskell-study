import System.Directory

main :: IO()
main = do
  let fileName = "./test.txt"
  let d1 = [Just 5, Nothing, Nothing, Just 8, Just 9] :: [Maybe Int]
  writeFile fileName (show d1)
  input <- readFile fileName
  let d2 = read input :: [Maybe Int]
  print (show d2)
  removeFile fileName

-- 自分の得意な言語で
-- Let's チャレンジ！！
data State = State
    {   temp :: Int
    ,   time :: Int
    ,   price :: Int
    } deriving (Show)

main = do
    _ <- getLine
    allX <- getContents
    let all = map extract (lines allX)
    putStrLn (show (func (State 0 0 0) all))

func :: State -> [(Int, Int)] -> Int
func (State _ 24 price) _ =
  price
func (State temp time price) ((aTime,bump):rest) =
  if time == aTime then
    func (State (newTemp + bump) (time + 1) newPrice) rest
  else
    func (State newTemp (time + 1) newPrice) ((aTime,bump):rest)
  where
    newTemp = if (temp - 1) > 0 then (temp - 1) else 0
    newPrice = (+) price $ if temp > 0 then 2 else 1

extract :: String -> (Int, Int)
extract str =
    (\(x,y) -> (read x, if tail y == "in" then 5 else 3)) $ break (== ' ') str

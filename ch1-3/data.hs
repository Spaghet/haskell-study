data BookInfo = Book Int String [String]
  deriving (Show)

data MagazineInfo = Magazine Int String [String]
  deriving (Show)

type CustomerID = Int
type Address = [String]

data Customer = Customer {
    customerID      ::  CustomerID
  , customerName    ::  String
  , customerAddress ::  Address
  } deriving (Show)

myInfo :: BookInfo
myInfo =  Book 9780135072455 "Algebra of Programming"
          ["Richard Bird", "Oege de Moor"]

bookID :: BookInfo -> Int
bookID (Book id _ _) = id

data List a = Cons a (List a)
  | Nil
  deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList (Cons x xs) = x : (toList xs)
toList Nil = []

data Tree a = Tree a (Maybe (Tree a)) (Maybe (Tree a))
  deriving (Show)

pluralise :: String -> [Int] -> [String]
pluralise word count = map plural count
  where
    plural 0 = "no " ++ word ++ "s"
    plural 1 = "one " ++ word
    plural n = show n ++ " " ++ word ++ "s"

{-# LANGUAGE TypeSynonymInstances #-}

data Maybe a = Nothing
             | Maybe a
             deriving (Show, Ord, Eq,Read)

 data Either a b = Left a
                 | Right b
                 deriving (Show, Read, Eq, Ord)

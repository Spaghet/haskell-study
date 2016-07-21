--module declaration
module Prettify
(
    Doc
  , (<>)
  , char
  , text
  , double
  , fsep
  , hcat
  , punctuate
  , compact
  , pretty
) where

data Doc  = Empty
          | Char Char
          | Text String
          | Line
          | Concat Doc Doc
          | Union Doc Doc
  deriving (Show, Eq)

--exports
--renderers
compact :: Doc -> String
compact x = transform [x]
  where transform []      = ""
        transform (d:ds)  =
          case d of
            Empty         -> transform ds
            Char c        -> c : transform ds
            Text t        -> t ++ transform ds
            Line          -> '\n' : transform ds
            a `Concat` b  -> transform (a:b:ds)
            _ `Union` b   -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) =
          case d of
            Empty         -> best col ds
            Char c        -> c : best (col + 1) ds
            Text t        -> t ++ best (col + length t) ds
            Line          -> '\n' : best (col + 1) ds
            a `Concat` b  -> best col (a:b:ds)
            a `Union` b   -> nicest col (best col (a:ds))
                                        (best col (b:ds))
        best _ _ = ""

        nicest col a b | (width - least) `fits` a = a
                       | otherwise                = b
                       where least = min width col
--constructors and other exports
empty :: Doc
empty = Empty

char :: Char -> Doc
char = Char

text :: String -> Doc
text "" = Empty
text str = Text str

double :: Double -> Doc
double num = text (show num)

hcat :: [Doc] -> Doc
hcat = fold (<>)

fsep :: [Doc] -> Doc
fsep = fold (</>)

--helper functions
fits :: Int -> String -> Bool
w `fits` _  | w < 0  = False
w `fits` ""         = True
w `fits` ('\n':_)   = True
w `fits` (c:cs)      = fits (w - 1) cs

line :: Doc
line = Line

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold func = foldr func empty

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
a <> b = a `Concat` b

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y)  = flatten x `Concat` flatten y
flatten Line            = Char ' '
flatten (x `Union` _)   = flatten x
flatten other           = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds

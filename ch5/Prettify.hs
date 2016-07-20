--module declaration
module Prettify
  (
  renderJValue
  ) where
--imports
import SimpleJSON

data Doc = ToBeDefined
  deriving (Show)


renderJValue :: JValue -> Doc
renderJValue (JBool True)   = text "true"
renderJValue (JBool False)  = text "false"
renderJValue (JNull)        = text "null"
renderJValue (JString s)    = string s
renderJValue (JNumber n)    = double n

string :: String -> Doc
string str = enclose '"' '"' . hcat . map oneChar

char :: Char -> Doc
char c = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

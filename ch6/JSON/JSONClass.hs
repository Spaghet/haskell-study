module JSONClass
  (
  JAry(..)
  )where

import SimpleJSON

type JSONError = String

class JSON a where
  toJValue    :: a -> JValue
  fromJValue  :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v)  = Right (f v)
doubleToJValue _ _            = Left "not a JSON Number"

instance JSON Int where
  toJValue    = JNumber . realToFrac
  fromJValue  = doubleToJValue round

instance JSON Integer where
  toJValue    = JNumber . realToFrac
  fromJValue  = doubleToJValue round

instance JSON Double where
  toJValue    = JNumber
  fromJValue  = doubleToJValue id

newtype JAry a = JAry {
  fromJAry :: [a]
  } deriving (Eq, Ord, Show)

instance (JSON a) => JSON [a] where
  toJValue    = undefined
  fromJValue  = undefined

instance (JSON a) => JSON [(String, a)]
  toJValue    = undefined
  fromJValue  = undefined

module Data.Three where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)

data Three = One | Two | Three

derive instance Generic Three _

instance Show Three where
  show x = genericShow x

instance Eq Three where
  eq x = genericEq x

instance Ord Three where
  compare x = genericCompare x

toInt :: Three -> Int
toInt One = 1
toInt Two = 2
toInt Three = 3
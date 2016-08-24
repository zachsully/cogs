module Data.Number.Rational where

import Data.Number.Natural

data Rational
  = Rat Natural Natural
  deriving (Show, Eq, Ord)

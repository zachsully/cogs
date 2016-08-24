module Data.Number.Complex where

import Data.Number.Real

import Prelude hiding (Real)

data Complex
  = R Real
  | I Real
  deriving (Show, Eq, Ord)

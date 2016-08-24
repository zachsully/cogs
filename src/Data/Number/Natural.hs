module Data.Number.Natural where

data Natural
  = Zero
  | Succ Natural
  deriving (Show, Eq, Ord)

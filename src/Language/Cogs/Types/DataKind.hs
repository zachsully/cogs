module Language.Cogs.Types.DataKind where

data Cogs
  = CNatural
  | CRational
  | CInteger
  | CReal
  | CComplex
  deriving (Show, Eq, Ord)

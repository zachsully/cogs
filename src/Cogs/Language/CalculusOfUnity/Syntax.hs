module Cogs.Language.CalculusOfUnity.Syntax where

import Data.Text

data Type =
  -- well formed
    Hash

  -- positive
  | PNeg Type
  | POne
  | PProd Type Type
  | PSum Type Type

  -- negative
  | NNeg Type
  | NProd Type Type
  | NBot
  | NSum Type Type

  -- shifted types
  | Posit Type
  | Neg Type
  deriving (Show,Eq)

data Term =
  -- positive
    PVar Text
  | PCont Term
  | PUnit
  | PPair Term Term
  | PInl Term
  | PInr Term

  -- negative
  | NVar Text
  | NExpr Term
  | NFst Term
  | NSnd Term
  | NEmpty
  | NSumPrj Term Term

  -- shifts
  | Capture Term
  | Suspend Term
  deriving (Show,Eq)

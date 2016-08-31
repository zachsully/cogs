----------------------------------------------------------------
--                                                    2016.08.31
-- |
-- Module      :  Language.Cogs.Types.DataKind
-- Copyright   :  Copyright (c) 2016 Zach Sullivan
-- License     :  MIT
-- Maintainer  :  zachsully@gmail.com
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Cogs Types
--
----------------------------------------------------------------

module Language.Cogs.Types.DataKind where

data Cogs
  = CNatural
  | CRational
  | CInteger
  | CReal
  | CComplex

  -- function typ
  | Cogs :-> Cogs
  deriving (Show, Eq, Ord)

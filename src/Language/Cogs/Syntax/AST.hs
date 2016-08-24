----------------------------------------------------------------
--                                                    2016.06.23
-- |
-- Module      :  Language.Cogs.Syntax.AST
-- Copyright   :  Copyright (c) 2016 Zach Sullivan
-- License     :  MIT
-- Maintainer  :  zachsully@gmail.com
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Main AST for Cogs
--
----------------------------------------------------------------

module Language.Cogs.Syntax.AST where

data AST =
    Nat
  | NaryOp [AST]
  deriving Show

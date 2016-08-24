----------------------------------------------------------------
--                                                    2016.08.24
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

data Term
  = Literal
  | NaryOp [Term]
  deriving Show

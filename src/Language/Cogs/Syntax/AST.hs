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

module Language.Cogs.Syntax.AST
  ( Term(..) )
  where

data Term a
  = Var Char
  | Literal a
  | (Term a) :+: (Term a)
  | (Term a) :*: (Term a)
  | (Term a) :/: (Term a)
  | (Term a) :^: (Term a)
  deriving (Show, Eq, Ord)

-- Originally from https://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
infix 4 :+:
infix 5 :*:, :/:
infix 6 :^:

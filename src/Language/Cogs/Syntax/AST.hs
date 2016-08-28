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
  ( Term(..)
  , Expr(..) )
  where

data Term a
  = Literal a
  | NaryOp [Term a]
  deriving Show

-- Originally from https://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
infix 4 :+:
infix 5 :*:, :/:
infix 6 :^:

data Expr a
  = Var Char
  | Const a
  | (Expr a) :+: (Expr a)
  | (Expr a) :*: (Expr a)
  | (Expr a) :/: (Expr a)
  | (Expr a) :^: (Expr a)
  deriving (Show, Eq, Ord)

----------------------------------------------------------------
--                                                    2016.08.28
-- |
-- Module      :  Language.Cogs.Evaluation.Simplify
-- Copyright   :  Copyright (c) 2016 Zach Sullivan
-- License     :  MIT
-- Maintainer  :  zachsully@gmail.com
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- An algebraic simplification of a Cogs program
--
----------------------------------------------------------------

module Language.Cogs.Evaluation.Simplify
  ( simplify ) where

import Language.Cogs.Syntax.AST

simplify :: (Num a, Eq a) => Term a -> Term a
simplify (Lit a)       = Lit a
simplify (Var a)       = Var a
simplify (Lit 0 :+: x) = simplify x
simplify (x :+: Lit 0) = simplify x
simplify (Lit 1 :*: x) = simplify x
simplify (x :*: Lit 1) = simplify x
simplify (x :/: Lit 1) = simplify x
simplify x             = simplify x

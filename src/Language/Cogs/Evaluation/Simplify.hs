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
simplify (Literal a)       = Literal a
simplify (Var a)           = Var a
simplify (Literal 0 :+: x) = x
simplify (x :+: Literal 0) = x
simplify (Literal 1 :*: x) = x
simplify (x :*: Literal 1) = x
simplify (x :/: Literal 1) = x
simplify x                 = x

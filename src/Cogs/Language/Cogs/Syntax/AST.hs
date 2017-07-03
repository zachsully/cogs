{-# LANGUAGE BangPatterns,
             DataKinds,
             GADTs,
             KindSignatures,
             RankNTypes #-}

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

import Language.Cogs.Types.DataKind

data Term a
  = Var String
  | Lit  a
  | Neg (Term a)
  | Term a :+: Term a
  | Term a :*: Term a
  | Term a :/: Term a
  | Term a :^: Term a
  | Sum  [a]
  | Prod [a]
  deriving (Show, Eq, Ord)



-- Originally from https://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html

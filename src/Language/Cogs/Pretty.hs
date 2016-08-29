{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------
--                                                    2016.08.28
-- |
-- Module      :  Language.Cogs.Pretty
-- Copyright   :  Copyright (c) 2016 Zach Sullivan
-- License     :  MIT
-- Maintainer  :  zachsully@gmail.com
-- Stability   :  experimental
-- Portability :  GHC-only
--
----------------------------------------------------------------

module Language.Cogs.Pretty
  ( pretty ) where

import Data.ByteString

import Language.Cogs.Syntax.AST

pretty :: Term a -> ByteString
pretty = undefined

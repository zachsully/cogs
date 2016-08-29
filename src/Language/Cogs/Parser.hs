{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------
--                                                    2016.08.24
-- |
-- Module      :  Language.Cogs.Parser
-- Copyright   :  Copyright (c) 2016 Zach Sullivan
-- License     :  MIT
-- Maintainer  :  zachsully@gmail.com
-- Stability   :  experimental
-- Portability :  GHC-only
--
----------------------------------------------------------------

module Language.Cogs.Parser
  ( parseCogs ) where

import Data.Attoparsec.ByteString
import Data.ByteString

import Language.Cogs.Syntax.AST

parseCogs :: ByteString -> Term a
parseCogs input =
  case parse parserCogs input of
    (Done _ x) -> x
    _          -> error "failed to parse input"


parserCogs :: Parser (Term a)
parserCogs = undefined

-- ops, types, names :: [String]
-- ops   = ["+","*","-","^"]
-- types = ["->"]
-- names = ["def", "fn", "if", "∞", "expect", "observe",
--          "return", "match", "integrate", "summate", "product",
--          "data", "import"]

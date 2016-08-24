{-# LANGUAGE OverloadedStrings #-}
module Cogs.Parse where

import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Data.Scientific (toBoundedInteger)

data Expr = T Term
          deriving Show

type Term = Int

parseData :: Parser Expr
parseData = do
  skipSpace
  n <- fromJust <$> toBoundedInteger <$> scientific
  return (T n)

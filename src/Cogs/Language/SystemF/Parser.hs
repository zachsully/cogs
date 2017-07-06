module Cogs.Language.SystemF.Parser where

import Cogs.Language.SystemF.Syntax

import Data.Text
import Text.Parsec
import Text.Parsec.Text

parseProg :: Text -> Either ParseError Term
parseProg = undefined

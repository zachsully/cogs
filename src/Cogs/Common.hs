{-# LANGUAGE OverloadedStrings #-}
module Cogs.Common where

import Control.Monad.Identity
import Data.Monoid
import Data.Text
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok

--------------------------------------------------------------------------------
--                                 PARSING                                    --
--------------------------------------------------------------------------------

syntaxDef :: Tok.GenLanguageDef Text () Identity
syntaxDef
  = Tok.LanguageDef
  { Tok.commentStart    = "(-"
  , Tok.commentEnd      = "-)"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = satisfy (\c -> elem c ['a'..'z']
                                      || elem c ['A'..'Z'])
  , Tok.identLetter     = satisfy (\c -> elem c ['a'..'z']
                                      || elem c ['A'..'Z'])
  , Tok.opStart         = oneOf ""
  , Tok.opLetter        = oneOf ""
  , Tok.reservedNames   = ["nat","λ","Λ","∀","∃","μ","Π","Σ"
                          ,"#","*",":",".","→","rec"]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.GenTokenParser Text () Identity
lexer = Tok.makeTokenParser syntaxDef

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

whiteSpace' :: Parser ()
whiteSpace' = whiteSpace <|> return ()


parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

identifier :: Parser String
identifier = Tok.identifier lexer

natural :: Parser Integer
natural = Tok.natural lexer

--------------------------------------------------------------------------------
--                                  PRETTY                                    --
--------------------------------------------------------------------------------

(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b

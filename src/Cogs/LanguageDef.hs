module Cogs.LanguageDef where

import Control.Monad.Identity
import Data.Text
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok

--------------------------------------------------------------------------------
--                                 PARSING                                    --
--------------------------------------------------------------------------------

class Parseable t where
  parseTerm :: Text -> t

syntaxDef :: Tok.GenLanguageDef Text () Identity
syntaxDef
  = Tok.LanguageDef
  { Tok.commentStart    = "(-"
  , Tok.commentEnd      = "-)"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = letter
  , Tok.opStart         = oneOf ""
  , Tok.opLetter        = oneOf ""
  , Tok.reservedNames   = ["nat","λ","Λ","μ","#","*",":",".","→","rec"]
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
--                             PRETTY PRINTING                                --
--------------------------------------------------------------------------------

class Pretty t where
  pretty :: t -> Text


--------------------------------------------------------------------------------
--                              TYPE CHECKING                                 --
--------------------------------------------------------------------------------

class (Pretty t) => Check t where
  checkClosedTerm :: t -> t

--------------------------------------------------------------------------------
--                               EVALUATION                                   --
--------------------------------------------------------------------------------

class (Pretty t) => Eval t where
  evalClosedTerm :: t -> t

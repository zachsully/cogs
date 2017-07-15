{-# LANGUAGE OverloadedStrings #-}
module Cogs.Common where

import Control.Monad.Identity
import Data.Monoid
import Data.Text
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as Tok

--------------------------------------------------------------------------------
--                               AST MAXIMUM                                  --
--------------------------------------------------------------------------------

parseExpr :: Text -> Either Text Expr
parseExpr p =
  case parse pExpr "" p of
    Left err -> Left . pack . show $ err
    Right r -> Right r

data Expr
  = Var_ Text
  | Arrow_ Expr Expr
  | Exists_ Expr Expr
  | Forall_ Expr Expr
  | Pi_ Expr Expr
  | Sigma_ Expr Expr
  | Lam_ Expr Expr
  | BigLam_ Expr Expr
  | Ann_ Expr Expr
  | App_ Expr Expr
  deriving (Show,Eq)

pExpr :: Parser Expr
pExpr =
  try pArrow_
  -- <|> pVar_
  <|> (pComment >> pExpr)
  <|> (pWhiteSpace >> pExpr)

pVar_ :: Parser Expr
pVar_ =
  do { v <- pack <$> many1 (oneOf (['a'..'z'] ++ ['A'..'Z']))
     ; return (Var_ v) }

pArrow_ :: Parser Expr
pArrow_ =
  chainl1 pVar_ (do { pWhiteSpace
                    ; _ <- char '\8594'
                    ; pWhiteSpace
                    ; return Arrow_ })

pWhiteSpace :: Parser ()
pWhiteSpace = many (oneOf [' ','\n','\t']) >> return ()

pComment :: Parser ()
pComment =
  do { _ <- string "(-"
     ; _ <- manyTill anyChar (try (string "-)"))
     ; return () }

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
                          ,"#","*",":",".","→","rec","="]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.GenTokenParser Text () Identity
lexer = Tok.makeTokenParser syntaxDef

tok :: Parser a -> Parser a
tok = Tok.lexeme lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

whiteSpace' :: Parser ()
whiteSpace' = whiteSpace <|> return ()

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = tok . Tok.reserved lexer

reserved' :: String -> Parser ()
reserved' = Tok.reserved lexer

identifier :: Parser String
identifier = tok (Tok.identifier lexer)

identifier' :: Parser String
identifier' = Tok.identifier lexer

natural :: Parser Integer
natural = tok (Tok.natural lexer)

--------------------------------------------------------------------------------
--                                  PRETTY                                    --
--------------------------------------------------------------------------------

(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b

--------------------------------------------------------------------------------
--                               TYPECHECKER                                  --
--------------------------------------------------------------------------------

type TypeCheckError = Text

module Cogs.Language.SystemT.Parser where

import Cogs.Language.SystemT.SystemT

import Data.Text hiding (unwords,foldr,replicate)
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Prim
import qualified Text.Parsec.Token as Tok
import Prelude hiding (elem)
import Control.Monad.Identity

parseProg :: Text -> Either ParseError Term
parseProg = parse pTerm ""

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
  , Tok.reservedNames   = ["nat","λ",":",".","→","rec"]
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.GenTokenParser Text () Identity
lexer = Tok.makeTokenParser syntaxDef

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

identifier :: Parser String
identifier = Tok.identifier lexer

natural :: Parser Integer
natural = Tok.natural lexer

pType :: Parser Type
pType =
  try (const Natural <$> string "nat")
  <|> (do ty1 <- pType
          spaces
          _ <- string "→"
          spaces
          ty2 <- pType
          return (Fun ty1 ty2))

pTerm :: Parser Term
pTerm =
  try (parens pTerm)
  <|> pNat
  <|> pVar
  <?> "term"
  -- <|> pLam
  -- <|> pRec
  -- <|> pApp

pVar :: Parser Term
pVar = Var . pack <$> identifier

pNat :: Parser Term
pNat = do
  whiteSpace
  n <- fromIntegral <$> natural
  case n >= 0 of
    True  -> return $ foldr (\s p -> s p) Zero (replicate n Succ)
    False -> parserFail . unwords $ ["constant",show n,"isn't a natural."]

pLam :: Parser Term
pLam = do
  reserved "λ"
  (Var v) <- pVar
  whiteSpace
  reserved ":"
  whiteSpace
  ty <- pType
  reserved "."
  whiteSpace
  t <- pTerm
  return (Lam v ty t)

pApp :: Parser Term
pApp = do
  t1 <- pTerm
  _ <- many (char ' ')
  t2 <- pTerm
  return (App t1 t2)

pRec :: Parser Term
pRec = do
  _ <- string "rec"
  t1 <- pTerm
  _ <- string "of"
  t2 <- pTerm
  _ <- char '|'
  t3 <- pTerm
  return (Rec t1 t2 t3)

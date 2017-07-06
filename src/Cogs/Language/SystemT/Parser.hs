module Cogs.Language.SystemT.Parser where

import Cogs.LanguageDef
import Cogs.Language.SystemT.Syntax

import Data.Text hiding (unwords,foldr,replicate)
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Prim
import Prelude hiding (elem)

parseProg :: Text -> Either ParseError Term
parseProg = parse pTerm ""

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
  chainl1 (whiteSpace' >>
            ( try pNat
              <|> pVar
              <|> pLam
              <|> (parens pTerm)
              <?> "term"
            )
          )
          (return App)

  -- <|> pRec
  -- <|> pApp

pVar :: Parser Term
pVar = (Var . pack <$> identifier) <?> "var"

pNat :: Parser Term
pNat = do
  n <- fromIntegral <$> natural
  case n >= 0 of
    True  -> return $ foldr (\s p -> s p) Zero (replicate n Succ)
    False -> parserFail . unwords $ ["constant",show n,"isn't a natural."]

pLam :: Parser Term
pLam =
  do { reserved "λ"
     ; (Var v) <- pVar
     ; whiteSpace'
     ; reserved ":"
     ; whiteSpace'
     ; ty <- pType
     ; whiteSpace'
     ; reserved "."
     ; whiteSpace'
     ; t <- pTerm
     ; return (Lam v ty t) }
  <?> "lam"

pApp :: Parser Term
pApp = do
  t1 <- pTerm
  whiteSpace'
  t2 <- pTerm
  return (App t1 t2)

pRec :: Parser Term
pRec = do
  reserved "rec"
  whiteSpace
  t1 <- pTerm
  whiteSpace
  _ <- string "of"
  t2 <- pTerm
  _ <- char '|'
  t3 <- pTerm
  return (Rec t1 t2 t3)

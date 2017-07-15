module Cogs.Language.SystemT.Parser where

import Cogs.Language.SystemT.Syntax

import Data.Text hiding (unwords,foldr,replicate)
import Text.Parsec
import Text.Parsec.Text
import Prelude hiding (elem)

parseProg :: Text -> Either ParseError Term
parseProg = parse pTerm ""

mWS :: Parser ()
mWS = many (char ' ') >> return ()

pType :: Parser Type
pType =
  try (const Natural <$> string "nat")
  <|> do { ty1 <- pType
         ; mWS
         ; _ <- char '\8594'
         ; mWS
         ; ty2 <- pType
         ; return (Fun ty1 ty2) }
  <?> "type"

pNonApp :: Parser Term
pNonApp =
  try (do { _ <- char '('
          ; mWS
          ; na <- pNonApp
          ; mWS
          ; _ <- char ')'
          ; return na })
  <|> pVar
  <|> pNat
  <|> pLam

pTerm :: Parser Term
pTerm =
  (try (chainl1 pNonApp (return App))
   <|> pNat
   <|> pVar
   <|> pLam)

pVar :: Parser Term
pVar = (Var . pack <$> many1 (oneOf (['a'..'z'] ++ ['A'..'Z']))) <?> "var"

pNat :: Parser Term
pNat = (read <$> many1 digit) >>= \n ->
       return $ foldr ($) Zero (replicate n Succ)

pLam :: Parser Term
pLam =
  do { _ <- char 'λ'
     ; mWS
     ; (Var v) <- pVar
     ; mWS
     ; _ <- char ':'
     ; mWS
     ; ty <- pType
     ; mWS
     ; _ <- char '.'
     ; mWS
     ; t <- pTerm
     ; return (Lam v ty t) }
  <?> "lam"

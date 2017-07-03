module Cogs.Language.SystemT.Parser where

import Cogs.Language.SystemT.SystemT

import Data.Text hiding (unwords,foldr,replicate)
import Text.Parsec.Text
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Prim
import Prelude hiding (elem)

parseProg :: Text -> Either ParseError Term
parseProg = parse pTerm ""

pTerm :: Parser Term
pTerm =
  try (skipComment >> pTerm)
  <|> pLam
  <|> pRec
  <|> pVar
  <|> pNat
  <|> pApp

skipComment :: Parser ()
skipComment = do
  spaces
  _ <- char '%'
  _ <- manyTill anyToken (char '\n')
  return ()

pVar :: Parser Term
pVar = Var . pack <$> many1 letter

pNat :: Parser Term
pNat = do
  n <- read <$> many1 digit
  case n >= 0 of
    True  -> return $ foldr (\s p -> s p) Zero (replicate n Succ)
    False -> parserFail . unwords $ ["constant",show n,"isn't a natural."]

pLam :: Parser Term
pLam = do
  _ <- char 'λ'
  (Var v) <- pVar
  _ <- many (string " ")
  _ <- char '.'
  _ <- many (string " ")
  t <- pTerm
  return (Lam v t)

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

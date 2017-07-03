module Cogs.Language.SystemT.Parser where

import Cogs.Language.SystemT.SystemT

import Data.ByteString.Char8
import Text.Parsec.ByteString
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.Prim
import Prelude hiding (elem)

parseProg :: ByteString -> Either ParseError Term
parseProg = parse pTerm ""

pTerm :: Parser Term
pTerm =
  try (skipComment >> pTerm)
  <|> pLam
  -- <|> pRec
  <|> pVar
  -- <|> pNat
  -- <|> pApp

skipComment :: Parser ()
skipComment = do
  spaces
  _ <- char '%'
  _ <- manyTill anyToken (char '\n')
  return ()

pVar :: Parser Term
pVar = Var . pack <$> many1 letter

pNat :: Parser Term
pNat = parserFail "todo nat"

pLam :: Parser Term
pLam = do
  _ <- char 'λ'
  (Var v) <- pVar
  _ <- many (char '_')
  _ <- char '.'
  spaces
  t <- pTerm
  return (Lam v t)

pApp :: Parser Term
pApp = parserFail "todo app"

pRec :: Parser Term
pRec = parserFail "todo rec"

module Cogs.Language.SystemT.Parser where

import Cogs.Common
import Cogs.Language.SystemT.Syntax

import Data.Text hiding (unwords,foldr,replicate)
import Text.Parsec
import Text.Parsec.Text
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
  <?> "type"

pTerm :: Parser Term
pTerm =
  chainl1 (whiteSpace' >>
            ( try pNat
              <|> pSucc
              <|> pVar
              <|> pLam
              <|> pRec
              <|> (parens pTerm)
              <?> "term"
            )
          )
          (return App)

pVar :: Parser Term
pVar = (Var . pack <$> identifier) <?> "var"

pNat :: Parser Term
pNat =
  do { n <- fromIntegral <$> natural
     ; case n >= 0 of
         True  -> return $ foldr (\s p -> s p) Zero (replicate n Succ)
         False -> parserFail . unwords $ ["constant",show n
                                         ,"isn't a natural."] }
  <?> "nat"

pSucc :: Parser Term
pSucc = do
  do { reserved "succ"
     ; whiteSpace
     ; t <- pTerm
     ; return (Succ t) }
  <?> "succ"

pLam :: Parser Term
pLam =
  do { reserved "λ"
     ; whiteSpace'
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

pRec :: Parser Term
pRec =
  do { reserved "rec"
     ; whiteSpace
     ; t1 <- pTerm
     ; whiteSpace
     ; _ <- string "{"
     ; whiteSpace'
     ; t2 <- pTerm
     ; whiteSpace'
     ; _ <- char '|'
     ; whiteSpace'
     ; t3 <- pTerm
     ; whiteSpace'
     ; return (Rec t1 t2 t3) }
  <?> "rec"

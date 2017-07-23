{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.Common.Parser where

import Cogs.Language.Common.Syntax

import Control.Monad.Identity
import Data.Monoid
import Data.Text
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Text
import Text.Parsec.Combinator

parseExpr :: Text -> Either Text Expr
parseExpr p =
  case parse pExpr "" p of
    Left err -> Left . pack . show $ err
    Right r -> Right r

--------------------------------------------------------------------------------
--                                 Parser                                     --
--------------------------------------------------------------------------------

pExpr :: Parser Expr
pExpr =
  try ((pComment >> pExpr) <* pWhiteSpace)
  <|> pLeftRec
  <?> "expr"

pWhiteSpace :: Parser ()
pWhiteSpace = many (oneOf " \n\t") >> return ()

pComment :: Parser ()
pComment =
  do { _ <- string "(-"
     ; _ <- manyTill anyChar (try (string "-)"))
     ; return () }
  <* pWhiteSpace

pParens :: Parser a -> Parser a
pParens p =
  do { _ <- char '('
     ; p' <- p
     ; _ <- char ')'
     ; return p' }

------------------------------
pNonLeftRec :: Parser Expr
pNonLeftRec =
  try pBinders
  <|> pNat_
  <|> pKHash_
  <|> pKStar_
  <|> pTyNat_
  <|> pVar_
  <|> ((pParens pExpr) <* pWhiteSpace)
  <?> "atomic expr"

pVar_ :: Parser Expr
pVar_ = Var_ . pack <$> many1 (oneOf (['a'..'z'] ++ ['A'..'Z'])) <* pWhiteSpace

pBinderVar_ :: Parser Expr
pBinderVar_ = Var_ . pack <$> many1 (oneOf []) <* pWhiteSpace

pNat_ :: Parser Expr
pNat_ = Nat_ . read <$> many1 digit <* pWhiteSpace

pTyNat_ :: Parser Expr
pTyNat_ = const TyNat_ <$> string "nat" <* pWhiteSpace

pKHash_ :: Parser Expr
pKHash_ = const KHash_ <$> char '#' <* pWhiteSpace

pKStar_ :: Parser Expr
pKStar_ = const KStar_ <$> char '*' <* pWhiteSpace

pBinders :: Parser Expr
pBinders =
  do { sym <- oneOf [lambdaLower,lambdaUpper,sigmaUpper,piUpper,muLower,nuLower
                    ,existsChar,forallChar]
     ; bind <- pExpr
     ; _ <- char '.'
     ; pWhiteSpace
     ; expr <- pExpr
     ; return $ Binder_ (pack [sym]) bind expr }


------------------------------
pLeftRec :: Parser Expr
pLeftRec =
  chainl1 pNonLeftRec
    (try pArrow_
     <|> pAnn_
     <|> pApp_
     <?> "left recursive expr")

pArrow_ :: Parser (Expr -> Expr -> Expr)
pArrow_ =
  do { pWhiteSpace
     ; _ <- char '\8594'
     ; pWhiteSpace
     ; return Arrow_ }

pAnn_ :: Parser (Expr -> Expr -> Expr)
pAnn_ =
  do { pWhiteSpace
     ; _ <- char ':'
     ; pWhiteSpace
     ; return Ann_ }

pApp_ :: Parser (Expr -> Expr -> Expr)
pApp_ = pWhiteSpace >> return App_

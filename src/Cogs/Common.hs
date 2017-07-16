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
  = Nat_ Integer
  | Var_ Text
  | TyNat_
  | KStar_
  | KHash_
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
  do { sym <- oneOf ['\955'  -- little lambda
                    ,'\923'  -- big lambda
                    ,'\931'  -- big sigma
                    ,'\928'  -- big pi
                    ,'\8707' -- exists
                    ,'\8704' -- forall
                    ]
     ; bind <- pExpr
     ; _ <- char '.'
     ; pWhiteSpace
     ; expr <- pExpr
     ; return $ (case sym of
                  '\955'  -> Lam_
                  '\923'  -> BigLam_
                  '\931'  -> Sigma_
                  '\928'  -> Pi_
                  '\8707' -> Exists_
                  '\8704' -> Forall_) bind expr }


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

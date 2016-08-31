{-# LANGUAGE OverloadedStrings #-}

----------------------------------------------------------------
--                                                    2016.08.24
-- |
-- Module      :  Language.Cogs.Parser
-- Copyright   :  Copyright (c) 2016 Zach Sullivan
-- License     :  MIT
-- Maintainer  :  zachsully@gmail.com
-- Stability   :  experimental
-- Portability :  GHC-only
--
----------------------------------------------------------------

module Language.Cogs.Parser
  ( parseCogs ) where

import           Data.Functor.Identity
import           Data.Text
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Expr  as Expr
import qualified Text.ParserCombinators.Parsec.Token as Tok

import Language.Cogs.Syntax.AST

parseCogs :: String -> Term Integer
parseCogs input =
  case parse whileParser "" input of
    Right x -> x
    Left e  -> error $ show e


languageDef :: GenLanguageDef String a Identity
languageDef =
  emptyDef { Tok.commentStart    = "{-"
           , Tok.commentEnd      = "-}"
           , Tok.commentLine     = "--"
           , Tok.identStart      = letter
           , Tok.identLetter     = alphaNum
           , Tok.reservedNames   = []
           , Tok.reservedOpNames = ["+","-","*","/"] }

lexer :: Tok.GenTokenParser String a Identity
lexer = Tok.makeTokenParser languageDef

-- primitive parsers
identifier :: Parser String
identifier = Tok.identifier lexer

reserved,reservedOp :: String -> Parser ()
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

integer :: Parser Integer
integer = Tok.integer lexer


whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer


whileParser :: Parser (Term Integer)
whileParser = whiteSpace >> term

term :: Parser (Term Integer)
term = try $ parens term
   <|> try literal

literal :: Parser (Term Integer)
literal = Literal <$> integer



-- table :: OperatorTable (Term a)
-- table =
--   [ [ op "*"  ]
--   , []
--   ]
--   where op s f assoc = Infix (string s >> return f) assoc

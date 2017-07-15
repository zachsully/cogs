{-# LANGUAGE OverloadedStrings #-}
module Cogs.Preprocessor where

import qualified Data.Text.IO as T
import Text.Parsec.Char
import Text.Parsec.Text
import Text.Parsec.Prim
import System.IO

data LanguageTag
  = SystemT      -- ^ simply typed lambda calculus with primitive recursion
  | PCF          -- ^ programming computable functions
  -- LAMBDA CUBE --
  | STLC         -- ^ simply typed lambda calculus
  | SystemF      -- ^ higher order lambda calculus
  | STLCOmega    -- ^ simply typed lambda calculus with type operators
  | SystemFOmega -- ^ higher order lambda calculus with type operators
  | DTLC         -- ^ dependently typed lambda calculus
  | DTLC2        -- ^ higher order dependently typed lambda calculus
  | CoC1         -- ^ dependently typed lambda calculus with type operators
  | CoC          -- ^ calculus of constructions

instance Show LanguageTag where
  show SystemT      = "λρ"
  show PCF          = "pcf"
  show STLC         = "λ"
  show SystemF      = "λ2"
  show STLCOmega    = "λω"
  show SystemFOmega = "λω2"
  show DTLC         = "λΠ"
  show DTLC2        = "λΠ2"
  show CoC1         = "λΠω"
  show CoC          = "λΠω2"

getLang :: FilePath -> IO LanguageTag
getLang "-" = return SystemT
getLang fp =
  withFile fp ReadMode $ \h ->
  T.hGetLine h >>= \l ->
  case parse pLanguage "" l of
    Left _ -> error "cannot deduce language"
    Right l'  -> return l'

pPragma :: String -> Parser ()
pPragma s = do
  _ <- string "(-# "
  _ <- string s
  _ <- string " #-)"
  return ()

pLanguage :: Parser LanguageTag
pLanguage =
  try (const SystemT      <$> pPragma "λρ")
  <|> (const PCF          <$> pPragma "pcf")
  <|> (const STLC         <$> pPragma "λ")
  <|> (const SystemF      <$> pPragma "λ2")
  <|> (const STLCOmega    <$> pPragma "λω")
  <|> (const SystemFOmega <$> pPragma "λω2")
  <|> (const DTLC         <$> pPragma "λΠ")
  <|> (const DTLC2        <$> pPragma "λΠ2")
  <|> (const CoC1         <$> pPragma "λΠω")
  <|> (const CoC          <$> pPragma "λΠω2")

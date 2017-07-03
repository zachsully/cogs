{-# LANGUAGE OverloadedStrings #-}
module Cogs.Preprocessor where

import qualified Data.ByteString as BS
import Text.Parsec.Char
import Text.Parsec.ByteString
import Text.Parsec.Prim
import System.IO

data Language
  = SystemT
  | PCF
  | SystemF
  deriving Show

getLang :: FilePath -> IO Language
getLang fp =
  withFile fp ReadMode $ \h ->
  BS.hGetLine h >>= \l ->
  case parse pLanguage "" l of
    Left err -> error "no lang"
    Right l  -> return l

pLanguage :: Parser Language
pLanguage =
  try (const SystemT <$> string "%system t")
  <|> (const SystemF <$> string "%system f")
  <|> (const PCF     <$> string "%pcf")

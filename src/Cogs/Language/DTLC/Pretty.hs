{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.DTLC.Pretty where

import Cogs.Language.DTLC.Syntax       

import Data.Monoid       
import Data.Text

ppType :: Type -> Text
ppType Natural        = "nat"
ppType (TyVar v)      = v
ppType (Pi v ty1 ty2) = "Π" <> v <> ":" <> ppType ty1 <> ". " <> ppType ty2

ppTerm :: Term -> Text
ppTerm = undefined

ppVal :: Val -> Text
ppVal = undefined

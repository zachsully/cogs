{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemT.Pretty where

import Cogs.Language.SystemT.SystemT

import Prelude hiding (unwords)
import Data.Monoid
import Data.Text

ppParens :: Text -> Text
ppParens t = "(" <> t <> ")"

ppType :: Type -> Text
ppType Natural = "nat"

ppTerm :: Term -> Text
ppTerm Zero     = "zero"
ppTerm (Succ t) = unwords ["succ",ppTerm t]

ppVal :: Val -> Text
ppVal (Nat _) = undefined
ppVal (Closure _ _ _) = undefined

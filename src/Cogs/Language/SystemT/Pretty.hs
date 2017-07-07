{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemT.Pretty where

import Cogs.Common
import Cogs.Language.SystemT.Syntax

import Prelude hiding (unwords)
import Data.Monoid
import Data.Text

ppParens :: Text -> Text
ppParens t = "(" <> t <> ")"

ppType :: Type -> Text
ppType Natural       = "nat"
ppType (Fun ty1 ty2) = unwords [ppType ty1,"→",ppType ty2]

ppTerm :: Term -> Text
ppTerm Zero           = "zero"
ppTerm (Succ Zero)    = unwords ["succ",ppTerm Zero]
ppTerm (Succ t)       = unwords ["succ",ppParens (ppTerm t)]
ppTerm (Var s)        = s
ppTerm (Lam s ty t)   = "λ" <> s <> ":" <> ppType ty <> "." <+> ppTerm t
ppTerm (App t1 t2)    = ppParens (ppTerm t1) <> " " <> ppTerm t2
ppTerm (Rec t1 t2 t3) =   "rec" <+> ppTerm t1
                      <+> "{" <+> ppTerm t2 <+> "|" <+> ppTerm t3 <+> "}"


ppVal :: Val -> Text
ppVal (Nat x) = pack . show $ go x
  where go Zero     = 0
        go (Succ z) = 1 + (go z)

ppVal c = pack . show $ c

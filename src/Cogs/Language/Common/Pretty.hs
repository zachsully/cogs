{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.Common.Pretty where

import Cogs.Language.Common.Syntax
import Data.Monoid
import Data.Text hiding (foldr)

--------------------------------------------------------------------------------
--                                  PRETTY                                    --
--------------------------------------------------------------------------------

ppExpr :: Expr -> Text
ppExpr (Nat_ i) = pack . show $ i
ppExpr (Var_ t) = t
ppExpr TyNat_ = "nat"
ppExpr KStar_ = "*"
ppExpr KHash_ = "#"
ppExpr (Arrow_ a b) = ppExpr a <+> "\8594" <+> ppExpr b
ppExpr (Binder_ v a b) = v <> ppExpr a <> "." <+> ppExpr b
ppExpr (Branching_ v a []) =   v <+> ppExpr a <+> "{}"
ppExpr (Branching_ v a (b:bs)) =
  v <+> ppExpr a
  <+> "{" <+> (foldr (\b' ss -> ppExpr b' <+> "|" <+> ss) (ppExpr b) bs)
  <+> "}"
ppExpr (Ann_ a b) = ppExpr a <+> ":" <+> ppExpr b
ppExpr (App_ a b) = "(" <> ppExpr a <> ")" <+> ppExpr b


(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b

{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.Common.Syntax where

import Data.Text

--------------------------------------------------------------------------------
--                               AST MAXIMUM                                  --
--------------------------------------------------------------------------------

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
  | Binder_ Text Expr Expr
  | Ann_ Expr Expr
  | App_ Expr Expr
  deriving (Show,Eq)

class CommonEncode a where
   from :: Expr -> Maybe a
   to :: a -> Expr

-- Haskell letters
lambdaLower,lambdaUpper,sigmaUpper,piUpper,existsLetter,forallLetter :: Char
lambdaLower  = '\955'
lambdaUpper  = '\923'
sigmaUpper   = '\931'
piUpper      = '\928'
existsLetter = '\8707'
forallLetter = '\8704'

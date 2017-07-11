{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.CoC.Syntax where

import Data.Monoid
import Data.Text

-- data Sort
--   = Box
--   | Star
--   | Type
--   deriving (Show,Eq)

data Expr =
    -- Naturals
    Zero
  | Succ Expr
  | Natural

    -- Term abstractions
  | App Expr Expr
  | Lam Text Expr Expr

    -- Type abstractions
  | Pi Text Expr Expr

    -- References
  | Var Text
  deriving (Show, Eq)

type Context = [(Text,Expr)]

-- Check that the context is well formed, that is, it only contains types     
ok :: Context -> Bool
ok [] = True
ok ((_,e):ctx) =
  case e of
    Pi _ _ _ -> ok ctx
    Natural -> ok ctx
    _ -> False

check :: Context -> Expr -> Either Text Expr
check ctx Zero = Right Natural
check ctx (Succ e) =
  case check ctx e of
    Right Natural -> Right Natural
    Right t -> Left ("succ expects a Natural, given " <> (pack . show $ t))
    Left err -> Left err

check ctx (Var s) =
  case lookup s ctx of
    Nothing -> Left (s <> " not in context")
    Just t -> Right t

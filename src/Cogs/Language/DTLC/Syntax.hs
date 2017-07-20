{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.DTLC.Syntax where

import Data.Text
import Data.Monoid

{- Need the environments to be well formed
,  everything is mutually recursive (types - terms - contexts)
,  lambdas now have a pi type

, use bidirectional typing (blog david ch.)
    -- divide into sythesizing and checking terms
    -- better error messages?
-}

data Expr =
  -- sorts
    Box
  -- kinds
  | Star
  -- types
  | Natural
  | Expr :-> Expr
  | Product Expr Expr
  | Pi Text Expr Expr
  -- terms
  | Var Text
  | Lam Text Expr Expr
  | App Expr Expr
  | Pair Expr Expr
  | Fst Expr
  | Snd Expr
  | Nat Int
  deriving (Show,Eq)

data Val
  = VNat Int
  | VClosure Text Expr Env
  deriving Show

data Env = Env [(Text,Expr,Env)]
  deriving Show

lookupEnv :: Text -> Env -> (Expr,Env)
lookupEnv s (Env []) = error . unpack $ "unbound var: " <> s
lookupEnv s (Env ((s',x,e'):rest)) = case s == s' of
                                       True -> (x,e')
                                       False -> lookupEnv s (Env rest)

extendEnv :: Text -> Expr -> Env -> Env -> Env
extendEnv s x e (Env e') = Env ((s,x,e):e')

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

data Type
  = Natural
  | TyVar Text
  | Pi Text Type Type
  deriving (Show, Eq)

data Term
  = Zero
  | Succ Term
  | Var Text
  | Lam Text Type Term
  | App Term Term
  deriving (Show, Eq)

data Val
  = Nat Term
  | Closure Text Term Env
  deriving Show

data Env = Env [(Text,Term,Env)]
  deriving Show

lookupEnv :: Text -> Env -> (Term,Env)
lookupEnv s (Env []) = error . unpack $ "unbound var: " <> s
lookupEnv s (Env ((s',x,e'):rest)) = case s == s' of
                                       True -> (x,e')
                                       False -> lookupEnv s (Env rest)

extendEnv :: Text -> Term -> Env -> Env -> Env
extendEnv s x e (Env e') = Env ((s,x,e):e')

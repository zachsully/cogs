{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemF.Syntax where

import Data.Text
import Data.Monoid       

data Type
  = Natural
  | TyVar Text
  | Fun Type Type
  | Forall Text Type
  deriving (Show, Eq)

data Term
  = Zero
  | Succ Term
  | Var Text
  | Lam Text Type Term
  | App Term Term
  | BigLam Text Term
  | TyApp Term Type
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

{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemFJ.Syntax where

import Data.Text
import Data.Monoid

data Kind
  = Star
  | KFun Kind Kind
  deriving (Show, Eq)

data Type
  = Natural
  | TyVar Text
  | Fun Type Type
  | Forall Text Type
  deriving (Show, Eq)

data Command = Cmd Value Kont
  deriving (Show,Eq)

data Kont
  = Ret
  | KTerm Term Kont
  | KType Type Kont
  deriving (Show,Eq)

data Term
  = Zero
  | Succ Term
  | Var Text
  | Lam Text Type Term
  | BigLam Text Kind Term
  | Mu Command
  deriving (Show, Eq)

data Value
  = Nat Term
  | Closure Text Term Env
  | TyClosure Text Type Env
  deriving (Show, Eq)

data Env = Env [(Text,Value,Env)]
  deriving (Show, Eq)

lookupEnv :: Text -> Env -> (Value,Env)
lookupEnv s (Env []) = error . unpack $ "unbound var: " <> s
lookupEnv s (Env ((s',x,e'):rest)) = case s == s' of
                                       True -> (x,e')
                                       False -> lookupEnv s (Env rest)

extendEnv :: Text -> Value -> Env -> Env -> Env
extendEnv s x e (Env e') = Env ((s,x,e):e')

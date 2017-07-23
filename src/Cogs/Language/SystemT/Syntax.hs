{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemT.Syntax where

import Cogs.Language.Common.Syntax       
import Prelude              hiding (pred,sum)
import Data.Text            hiding (foldr,replicate,unwords)

data Type
  = Natural
  | Fun Type Type
  deriving (Show, Eq)

data Term
  = Var Text
  | Zero
  | Succ Term
  | Lam Text Type Term
  | App Term Term
  | Rec Term Term Term
  deriving Show

data Env = Env [(Text,Term,Env)]
  deriving Show

lookupEnv :: Text -> Env -> (Term,Env)
lookupEnv s (Env []) = error $ "unbound var: " ++ show s
lookupEnv s (Env ((s',x,e'):rest)) = case s == s' of
                                       True -> (x,e')
                                       False -> lookupEnv s (Env rest)

extendEnv :: Text -> Term -> Env -> Env -> Env
extendEnv s x e (Env e') = Env ((s,x,e):e')

data Val
  = Nat Term
  | Closure Text Term Env
  deriving Show

--------------------------------------------------------------------------------
--                                 COMMON                                     --
--------------------------------------------------------------------------------

instance CommonEncode Term where
  from (Nat_ n) = Just (go n)
    where go 0 = Zero
          go n = Succ (go (n-1))
  from (Var_ t) = Just (Var t)
  from (Binder_ lambdaLower _ _) = undefined
  from (Ann_ _ _) = undefined
  from (App_ _ _) = undefined
  from _ = Nothing
  to (Succ _) = undefined
  to Zero = Nat_ 0

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

instance CommonEncode Type where
  from TyNat_ = Just Natural
  from (Arrow_ a b) =
    do { a' <- from a
       ; b' <- from b
       ; return (Fun a' b') }
  from _ = Nothing

  to Natural = TyNat_
  to (Fun a b) = Arrow_ (to a) (to b)

instance CommonEncode Term where
  from (Nat_ n) = Just (go n)
    where go 0 = Zero
          go n = Succ (go (n-1))
  from (Var_ t) = Just (Var t)
  from (Binder_ q (Ann_ (Var_ x) ty) e) =
    case (unpack q) == [lambdaLower] of
      True -> do { ty' <- from ty
                 ; e' <- from e
                 ; return (Lam x ty' e') }
      False -> Nothing
  from (Branching_ "rec" f (a:b:[])) =
    do { f' <- from f
       ; a' <- from a
       ; b' <- from b
       ; return (Rec f' a' b') }
  from (App_ a b) =
    do { a' <- from a
       ; b' <- from b
       ; return (App a' b') }
  from _ = Nothing

  to (Var t) = Var_ t
  to Zero = Nat_ 0
  to (Succ x) = case to x of
                  (Nat_ i) -> Nat_ (i+1)
                  _ -> error $ "ill formed term " ++ show x
  to (Lam x ty e) = Binder_ (pack [lambdaLower]) (Ann_ (Var_ x) (to ty)) (to e)
  to (App a b) = App_ (to a) (to b)
  to (Rec f a b) = Branching_ "rec" (to f) [to a,to b]

instance CommonEncode Val where
  from (Nat_ t) = Nothing
  from _ = Nothing
  to (Nat t) = to t
  to _ = undefined

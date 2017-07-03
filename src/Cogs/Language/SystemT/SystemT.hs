{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemT.SystemT where

import Prelude          hiding (pred,sum)
import Data.ByteString         (ByteString(..),unpack)

data Term
  = Var ByteString
  | Zero
  | Succ Term
  | Lam ByteString Term
  | App Term Term
  | Rec Term Term Term
  deriving Show

data Val
  = Nat Term
  | Closure ByteString Term Env
  deriving Show

prettyVal :: Val -> String
prettyVal (Nat x) = show $ go x
  where go Zero     = 0
        go (Succ z) = 1 + (go z)

data Env = Env [(ByteString,Term,Env)]
  deriving Show

lookupEnv :: ByteString -> Env -> (Term,Env)
lookupEnv s (Env []) = error $ "unbound var: " ++ show s
lookupEnv s (Env ((s',x,e'):rest)) = case s == s' of
                                       True -> (x,e')
                                       False -> lookupEnv s (Env rest)

extendEnv s x e (Env e') = Env ((s,x,e):e')

--------------------------------------------------------------------------------
--                                EVALUATION                                  --
--------------------------------------------------------------------------------

evalClosedTerm = eval (Env [])

eval :: Env -> Term -> Val
eval e (Var s)        = case lookupEnv s e of
                          (x,e') -> eval e' x
eval _ Zero           = Nat Zero

eval e (Succ x)       = case eval e x of
                          Nat x' -> Nat (Succ x')
                          _ -> error "type error"

eval e (Lam s x)      = Closure s x e

eval e (App x1 x2)    = case eval e x1 of
                          Closure s' x1' e1' -> eval (extendEnv s' x1' e1' e) x2
                          _ -> error "type error"

eval e (Rec x c1 c2)  = case eval e x of
                          Nat Zero -> eval e c1
                          Nat (Succ x') ->
                            case c2 of
                              (Lam a (Lam b t)) ->
                                let e'  = extendEnv a x' e e
                                    x'' = (Rec x' c1 c2)
                                    e'' = extendEnv b x'' e' e'
                                in eval e'' t
                              _ -> error "type error"
                          _ -> error "type error"


--------------------------------------------------------------------------------
--                                Programs                                    --
--------------------------------------------------------------------------------

foo1 = Rec (Succ (Succ (Succ (Succ (Succ Zero)))))
           Zero
           (Lam "x" (Lam "r" (Var "x")))

foo2 = Rec (Succ (Succ (Succ (Succ (Succ Zero)))))
           Zero
           (Lam "x" (Lam "r" (Var "r")))

-- MACROS
nat x = foldr (\s p -> s p) Zero (replicate x Succ)
pred x = Rec x Zero (Lam "x" (Lam "r" (Var "x")))
sum x y = Rec x y (Lam "x" (Lam "r" (Succ (Var "r"))))

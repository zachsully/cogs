{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemF.Evaluate where

import Cogs.Language.SystemF.Syntax

--------------------------------------------------------------------------------
--                                EVALUATION                                  --
--------------------------------------------------------------------------------

evalClosedTerm :: Term -> Val
evalClosedTerm = eval (Env [])

eval :: Env -> Term -> Val
eval e (Var s)        = case lookupEnv s e of
                          (x,e') -> eval e' x

eval _ Zero           = Nat Zero

eval e (Succ t)       = case eval e t of
                          Nat x' -> Nat (Succ x')

eval e (Lam s _ t)      = Closure s t e

eval e (App x1 x2)    = case eval e x1 of
                          Closure s' x1' e1' -> eval (extendEnv s' x2 e1' e) x1'

eval e (BigLam _ t)   = eval e t
eval e (TyApp t _)    = eval e t

--------------------------------------------------------------------------------
--                                 PROGRAMS                                   --
--------------------------------------------------------------------------------

foo0 = BigLam "y" (Lam "x" (TyVar "y") Zero)
foo1 = TyApp (BigLam "x" (Lam "x" Natural (Var "x"))) Natural
foo2 = App (TyApp (BigLam "x" (Lam "x" Natural (Var "x"))) Natural) Zero
foo3 = TyApp (Lam "x" Natural (Var "x")) Natural

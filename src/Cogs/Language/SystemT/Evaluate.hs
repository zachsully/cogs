{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemT.Evaluate where

import Cogs.Language.SystemT.Syntax

import Prelude              hiding (pred,sum)
import Data.Monoid
import Data.Text            hiding (foldr,replicate,unwords)
import Control.Monad.State

--------------------------------------------------------------------------------
--                                EVALUATION                                  --
--------------------------------------------------------------------------------

evalClosedTerm :: Term -> Val
evalClosedTerm = eval (Env [])

eval :: Env -> Term -> Val
eval e (Var s)        = case lookupEnv s e of
                          (x,e') -> eval e' x
eval _ Zero           = Nat Zero

eval e (Succ x)       = case eval e x of
                          Nat x' -> Nat (Succ x')
                          _ -> error "type error"

eval e (Lam s _ x)    = Closure s x e

eval e (App x1 x2)    = case eval e x1 of
                          Closure s' x1' e1' -> eval (extendEnv s' x1' e1' e) x2
                          _ -> error "type error"

eval e (Rec x c1 c2)  = case eval e x of
                          Nat Zero -> eval e c1
                          Nat (Succ x') ->
                            case c2 of
                              (Lam a _ (Lam b _ t)) ->
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
           (Lam "x" Natural (Lam "r" Natural (Var "x")))

foo2 = Rec (Succ (Succ (Succ (Succ (Succ Zero)))))
           Zero
           (Lam "x" Natural (Lam "r" Natural (Var "r")))

-- MACROS
nat x = foldr (\s p -> s p) Zero (replicate x Succ)
pred x = Rec x Zero (Lam "x" Natural (Lam "r" Natural (Var "x")))
sum x y = Rec x y (Lam "x" Natural (Lam "r" Natural (Succ (Var "r"))))

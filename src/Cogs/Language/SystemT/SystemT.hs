{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemT.SystemT where

import Prelude              hiding (pred,sum)
import Data.Monoid
import Data.Text            hiding (foldr,replicate,unwords)
import Control.Monad.State

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

data Val
  = Nat Term
  | Closure Text Term Env
  deriving Show

prettyVal :: Val -> String
prettyVal (Nat x) = show $ go x
  where go Zero     = 0
        go (Succ z) = 1 + (go z)

data Env = Env [(Text,Term,Env)]
  deriving Show

lookupEnv :: Text -> Env -> (Term,Env)
lookupEnv s (Env []) = error $ "unbound var: " ++ show s
lookupEnv s (Env ((s',x,e'):rest)) = case s == s' of
                                       True -> (x,e')
                                       False -> lookupEnv s (Env rest)

extendEnv s x e (Env e') = Env ((s,x,e):e')

--------------------------------------------------------------------------------
--                                  CHECK                                     --
--------------------------------------------------------------------------------

data TyEnv = TyEnv [(Text,Type)]
  deriving Show

lookupTyEnv :: Text -> TyEnv -> Type
lookupTyEnv s (TyEnv []) = error $ unpack (pack "unbound var: " <> s)
lookupTyEnv s (TyEnv ((s',t):rest)) = case s == s' of
                                         True -> t
                                         False -> lookupTyEnv s (TyEnv rest)

extendTyEnv s t (TyEnv e) = TyEnv ((s,t):e)
checkClosedTerm t = fst $ runState (check t) (TyEnv [])

check :: Term -> State TyEnv Type
check (Var s)  = lookupTyEnv s <$> get
check Zero     = return Natural
check (Succ t) =
  check t >>= \ty ->
  case ty of
    Natural -> return Natural
    ty' -> error $ "Succ : Natural -> Natural, given " ++ show ty'

check (Lam s domTy t) =
  do e <- get
     put (extendTyEnv s domTy e)
     codTy <- check t
     return (Fun domTy codTy)

check (App t1 t2) =
  check t1 >>= \ty1 ->
  case ty1 of
    Fun domTy codTy ->
      check t2 >>= \ty2 ->
      case domTy == ty2 of
        True -> return codTy
        False -> error $ unwords ["Application expects and argument of"
                                 ,show domTy,", given",show ty2]
    t -> error $ "Application requires a function, given " ++ show t

check (Rec t1 t2 t3) =
  check t1 >>= \ty1 ->
  case ty1 of
    Natural ->
      check t2 >>= \ty2 ->
      check t3 >>= \ty3 ->
      case ty3 of
        Fun Natural (Fun Natural domTy) -> return domTy
        t -> error $ unwords ["Rec requires that the second branch be"
                             ,"Fun Natural (Fun Natura a),","given"
                             ,show ty3]
    t -> error $ unwords ["Rec requires a Natural for its first arg, given"
                         ,show t]


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

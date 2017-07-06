{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemT.TypeCheck where

import Cogs.Language.SystemT.Syntax

import Prelude              hiding (pred,sum)
import Data.Monoid
import Data.Text            hiding (foldr,replicate,unwords)
import Control.Monad.State

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

extendTyEnv :: Text -> Type -> TyEnv -> TyEnv
extendTyEnv s ty (TyEnv e) = TyEnv ((s,ty):e)

checkClosedTerm :: Term -> Type
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
        Fun Natural (Fun Natural domTy) ->
          case domTy == ty2 of
            True -> return domTy
            False -> error $ unwords ["Branches of rec must return the same type"
                                     ,show ty2, "=/=",show domTy]
        _ -> error $ unwords ["Rec requires that the second branch be"
                             ,"Fun Natural (Fun Natura a),","given"
                             ,show ty3]
    t -> error $ unwords ["Rec requires a Natural for its first arg, given"
                         ,show t]

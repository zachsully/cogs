{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.DTLC.TypeCheck where

import Cogs.Language.DTLC.Syntax

import Control.Monad.State
import Data.Text
import Data.Monoid
import Prelude hiding (unwords)

data TyEnv = TyEnv [(Text,Type)]
  deriving Show

lookupTyEnv :: Text -> TyEnv -> Type
lookupTyEnv s (TyEnv []) = error . unpack $ "unbound var: " <> s
lookupTyEnv s (TyEnv ((s',t):rest)) = case s == s' of
                                         True -> t
                                         False -> lookupTyEnv s (TyEnv rest)

extendTyEnv :: Text -> Type -> TyEnv -> TyEnv
extendTyEnv s t (TyEnv e) = TyEnv ((s,t):e)

checkClosedTerm :: Term -> Type
checkClosedTerm t = fst $ runState (check t) (TyEnv [])

--------------------------------------------------------------------------------
--                                  CHECK                                     --
--------------------------------------------------------------------------------
{- A Bidirectional type checker
-}

check :: Term -> State TyEnv Type
check Zero = return Natural
check (Succ t) =
  check t >>= \ty ->
  case ty of
    Natural -> return Natural
    ty -> error $ "Succ : Natural -> Natural, given " ++ show ty

check (Var s)  = lookupTyEnv s <$> get
check (Lam s ty t) =
  do e <- get
     put (extendTyEnv s ty e)
     codTy' <- check t
     return (Pi s ty codTy')

check (App t1 t2) =
  check t1 >>= \ty1 ->
  case ty1 of
    Pi _ _ _ -> undefined
      -- check t2 >>= \ty2 -> return (unify domTy ty2)
    t -> error $ "Application requires a function, given " ++ show t

check t = error $ show t

subst :: Text -> Type -> TyEnv -> TyEnv
subst _ _  (TyEnv [])           = TyEnv []
subst s ty (TyEnv ((s',ty'):e)) =
  let hd = case s == s' of
             True  -> (s,ty)
             False -> (s',ty')
      tl = case subst s ty (TyEnv e) of
             TyEnv e' -> e'
  in TyEnv (hd:tl)

-- unify :: Type -> Type -> Type
-- unify Natural     Natural     = Natural
-- unify (Fun a1 b1) (Fun a2 b2) = Fun (unify a1 a2) (unify b1 b2)
-- unify (TyVar a1)  (TyVar a2)  =
--   case a1 == a2 of
--     True -> TyVar a1
--     False -> error . unpack $ unwords ["Cannot unify",a1,"and",a2]

-- unify (TyVar _)  b           = b
-- unify a          (TyVar _)   = a
-- unify a          b           = error . unpack $ unwords
--                                               ["Cannot unify",pack . show $ a
--                                               ,"and", pack . show $ b]

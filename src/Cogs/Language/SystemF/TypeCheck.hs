{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemF.TypeCheck where

import Cogs.Language.SystemF.Syntax

import Control.Monad.State
import Data.Text
import Data.Monoid
import Prelude hiding (unwords)

data Context = Context [(Text,Type)]
  deriving Show

lookupContext :: Text -> Context -> Type
lookupContext s (Context []) = error . unpack $ "unbound var: " <> s
lookupContext s (Context ((s',t):rest)) = case s == s' of
                                         True -> t
                                         False -> lookupContext s (Context rest)

extendContext :: Text -> Type -> Context -> Context
extendContext s t (Context e) = Context ((s,t):e)

checkClosedTerm :: Term -> Either TypeCheckError Type
checkClosedTerm t = check (Context [])

--------------------------------------------------------------------------------
--                                  CHECK                                     --
--------------------------------------------------------------------------------

-- | System F's typechecker needs to keep track of the bindings of type
--   variables to types and the bindings of the type parameters to type
--   variables
check :: Context -> Term -> Either TypeCheckError Type
check Zero = return Natural
check (Succ t) =
  check t >>= \ty ->
  case ty of
    Natural -> return Natural
    ty -> error $ "Succ : Natural -> Natural, given " ++ show ty

check (Var s)  = lookupContext s <$> get
check (Lam s ty t) =
  do e <- get
     put (extendContext s ty e)
     codTy' <- check t
     return (Fun ty codTy')

check (App t1 t2) =
  check t1 >>= \ty1 ->
  case ty1 of
    Fun domTy codTy ->
      check t2 >>= \ty2 -> return (unify domTy ty2)
    t -> error $ "Application requires a function, given " ++ show t

check (BigLam s t) =
  do e <- get
     put (extendContext s (TyVar s) e)
     ty <- check t
     return (Forall s ty)

check (TyApp t ty) =
  check t >>= \ty ->
  case ty of
    Forall s t' -> do
      e <- get
      put (subst s ty e)
      return t'
    t -> error $ "Type application requires a universal quantifier, given "
              ++ show t

check t = error $ show t

subst :: Text -> Type -> Context -> Context
subst _ _  (Context [])           = Context []
subst s ty (Context ((s',ty'):e)) =
  let hd = case s == s' of
             True  -> (s,ty)
             False -> (s',ty')
      tl = case subst s ty (Context e) of
             Context e' -> e'
  in Context (hd:tl)

unify :: Type -> Type -> Type
unify Natural     Natural     = Natural
unify (Fun a1 b1) (Fun a2 b2) = Fun (unify a1 a2) (unify b1 b2)
unify (TyVar a1)  (TyVar a2)  =
  case a1 == a2 of
    True -> TyVar a1
    False -> error . unpack $ unwords ["Cannot unify",a1,"and",a2]

unify (TyVar _)  b           = b
unify a          (TyVar _)   = a
unify a          b           = error . unpack $ unwords
                                              ["Cannot unify",pack . show $ a
                                              ,"and", pack . show $ b]

{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.DTLC.TypeCheck where

import Cogs.Language.DTLC.Syntax

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

checkClosedTerm :: Term -> Either Text Type
checkClosedTerm = check (Context [])

--------------------------------------------------------------------------------
--                                  CHECK                                     --
--------------------------------------------------------------------------------
{-                      A Bidirectional type checker                          -}


check :: Context -> Term -> Either Text Type
check = undefined

infer :: Context -> Term -> Either Text Type
infer = undefined

subst :: Text -> Type -> Context -> Context
subst _ _  (Context [])           = Context []
subst s ty (Context ((s',ty'):e)) =
  let hd = case s == s' of
             True  -> (s,ty)
             False -> (s',ty')
      tl = case subst s ty (Context e) of
             Context e' -> e'
  in Context (hd:tl)

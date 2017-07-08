{-# LANGUAGE OverloadedStrings #-}
module Cogs.Language.SystemT.TypeCheck where

import Cogs.Common
import Cogs.Language.SystemT.Syntax
import Cogs.Language.SystemT.Pretty

import Prelude              hiding (pred,sum,unwords)
import Data.Monoid
import Data.Text            hiding (foldr,replicate)

data Context = Context [(Text,Type)]
  deriving Show

lookupContext :: Text -> Context -> Type
lookupContext s (Context []) = error $ unpack (pack "unbound var: " <> s)
lookupContext s (Context ((s',t):rest)) = case s == s' of
                                         True -> t
                                         False -> lookupContext s (Context rest)

extendContext :: Text -> Type -> Context -> Context
extendContext s ty (Context e) = Context ((s,ty):e)

checkClosedTerm :: Term -> Either TypeCheckError Type
checkClosedTerm = check (Context [])

--------------------------------------------------------------------------------
--                                  CHECK                                     --
--------------------------------------------------------------------------------

check :: Context -> Term -> Either TypeCheckError Type
check c (Var s)  = Right (lookupContext s c)
check _ Zero     = Right Natural
check c (Succ t) =
  case check c t of
    Left err      -> Left err
    Right Natural -> Right Natural
    Right ty'     -> Left ("succ : nat -> nat, given " <> ppType ty')

check c (Lam s domTy t) =
  case check (extendContext s domTy c) t of
    Left err    -> Left err
    Right codTy -> return (Fun domTy codTy)

check c (App t1 t2) =
  case check c t1 of
    Left err -> Left err
    Right (Fun domTy codTy) ->
      case check c t2 of
        Left err -> Left err
        Right ty2 ->
          case domTy == ty2 of
            True -> Right codTy
            False -> Left (unwords ["Application expects and argument of"
                                   ,ppType domTy,", given",ppType ty2])
    Right ty1 -> Left ("Application requires a function, given " <> ppType ty1)

check c (Rec t1 t2 t3) =
  case check c t1 of
    Left err -> Left err
    Right Natural ->
      case (check c t2, check c t3) of
        (Left err, _) -> Left err
        (_, Left err) -> Left err
        (Right ty2, Right (Fun Natural (Fun Natural codTy3))) ->
          case ty2 == codTy3 of
            True -> Right ty2
            False -> Left (unwords ["Branches of rec must return the same type"
                                   ,ppType ty2, "=/=",ppType codTy3])
        (_,Right ty3) -> Left (unwords ["Rec requires that the second branch be"
                                       ,"Fun Natural (Fun Natura a),","given"
                                       ,ppType ty3])
    Right ty1 -> Left (unwords ["'rec' requires a Natural for its first arg, given"
                               ,ppType ty1])

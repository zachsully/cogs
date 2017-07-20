{-# LANGUAGE OverloadedStrings,
             TypeOperators #-}
module Cogs.Language.LambdaOmega.Syntax where

import Data.Text
import Data.Monoid hiding (Product)
import Prelude hiding (unwords)

type a :+: b = Either a b
type Error = Text

data Expr
  = Ann Expr Expr
  -- sorts
  | Box
  -- kinds
  | Star
  -- types
  | Natural
  | Expr :-> Expr
  | Product Expr Expr
  -- terms
  | Var Text
  | Lam Expr Expr
  | App Expr Expr
  | Pair Expr Expr
  | Fst Expr
  | Snd Expr
  | Nat Int
  deriving (Show,Eq)

eshow :: Expr -> Text
eshow = pack . show


isType :: Context -> Expr -> Bool
isType c e =
  case e of
    -- (Var _) -> Nothing
    -- (Lam _ _) -> Nothing
    -- (App _ _) -> Nothing
    Natural -> True
    (e' :-> _) -> isType c e
    (Product _ _) -> True
    _ -> False

isKind :: Expr -> Bool
isKind e =
  case e of
    Star -> True
    (Star :-> _) -> True
    _ -> False

type Context = [(Text,Expr)]

inferClosedExpr :: Expr -> Error :+: Expr
inferClosedExpr = infer []

checkClosedExpr :: Expr -> Expr -> Error :+: ()
checkClosedExpr = check []

infer :: Context -> Expr -> Error :+: Expr
infer c (Ann e0 e1) =
  case check c e0 e1 of
    Left err -> Left err
    Right () -> Right e1

infer _ Box  = Left "infer Box is undefined"
infer _ Star = Right Box
infer _ Natural = Right Star
infer c (e0 :-> e1) =
  case infer c e0 of
    Left err0 -> Left err0
    Right ty0 ->
      case infer c e1 of
        Left err1 -> Left err1
        Right ty1 ->
          case isKind ty0 && isKind ty1 of
            True -> Right Star
            False -> Right Box

infer c (Product e0 e1) =
  case check c e0 Star of
    Left err0 -> Left err0
    Right () ->
      case check c e1 Star of
        Left err1 -> Left err1
        Right () -> Right Star

infer c (Var v) =
  case lookup v c of
    Nothing -> Left "unbound var"
    Just e -> Right e

infer c (Lam e0 e1) =
  case e0 of
    (Var v) -> Left $ "cannot deduce type of " <> v
    (Ann (Var v) ty0) ->
      case infer ((v,ty0):c) e1 of
        Left err -> Left err
        Right ty1 -> Right (ty0 :-> ty1)
    _ -> Left "must bind a var or annotated var"

infer c (App e0 e1) =
  case infer c e0 of
    Left err -> Left err
    Right (e00 :-> e01) ->
      case check c e1 e00 of
        Left err -> Left err
        Right () -> Right e01
    Right _ -> Left "can only apply term and type functions"

infer c (Pair e0 e1) =
  case infer c e0 of
    Left err -> Left err
    Right ty0 ->
      case infer c e1 of
        Left err -> Left err
        Right ty1 ->
          case isType c ty0 && isType c ty1 of
            True -> Right (Pair ty0 ty1)
            False -> Left "pairs must contain terms"

infer c (Fst e) =
  case check c e Natural of
    Left err -> Left err
    Right () -> Right Natural

infer c (Snd e) =
  case check c e Natural of
    Left err -> Left err
    Right () -> Right Natural

infer _ (Nat _) = Right Natural

{- Check that given a context, an expr, and an expr with a type or kind, either
-- give an error or succeed
-}
check :: Context -> Expr -> Expr -> Error :+: ()
check c e ty =
  case infer c e of
    Left err -> Left err
    Right ty' ->
      case ty == ty' of
        True -> Right ()
        False -> Left $ unwords ["expected",eshow ty," given ",eshow ty']

-- Beta reduce types
-- this implements the conversion rule
normalizeTypes :: Context -> Expr -> Expr
normalizeTypes _ Box = Box
normalizeTypes _ Star = Star
normalizeTypes _ Natural = Natural
normalizeTypes c (e0 :-> e1) = (normalizeTypes c e0) :-> (normalizeTypes c e1)
normalizeTypes c (Product e0 e1) = Product (normalizeTypes c e0)
                                           (normalizeTypes c e1)
normalizeTypes c (Var v) = undefined
normalizeTypes c (Lam e0 e1) = undefined
normalizeTypes c (App e0 e1) = undefined
normalizeTypes c (Pair e0 e1) = Pair (normalizeTypes c e0) (normalizeTypes c e1)
normalizeTypes c (Fst e) = Fst (normalizeTypes c e)
normalizeTypes c (Snd e) = Snd (normalizeTypes c e)
normalizeTypes _ (Nat i) = Nat i

--------------------------------------------------------------------------------
--                                  TESTS                                     --
--------------------------------------------------------------------------------

foo0,foo1,foo2,foo3,foo4,foo5,foo6,foo7,foo8,foo9,foo10 :: Expr
foo0 = Lam (Var "foo") (Nat 3)
foo1 = Star :-> Star
foo2 = Natural :-> Natural
foo3 = Product Natural Natural
foo4 = Lam (Ann (Var "x") Star) (Product (Var "x") (Var "x"))
foo5 = Pair Natural Natural -- should fail
foo6 = Pair (Nat 42) (Nat 42)
foo7 = Pair (Lam (Ann (Var "x") Natural) (Var "x")) (Nat 3)
foo8 = App (Lam (Var "x") (Var "x")) (Nat 3)
foo9 = App (Lam (Ann (Var "x") Star) ((Var "x") :-> Natural)) Natural
foo10 = App (Lam (Var "x") (Product (Var "x") Natural)) Natural
-- need to normalize type
foo11 = Lam (Ann (Var "y") (App (Lam (Ann (Var "x") Star)
                                     (Product (Var "x") Natural))
                                Natural))
            (Var "y")
foo12 = Fst (Nat 42)
foo13 = Fst (Lam (Ann (Var "f") Natural) (Nat 42)) -- should fail

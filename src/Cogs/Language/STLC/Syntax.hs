{-# LANGUAGE FlexibleInstances,
             GADTs,
             MultiParamTypeClasses,
             OverloadedStrings,
             TypeOperators         #-}
module Cogs.Language.STLC.Syntax where

import Data.Text
import Data.Monoid

data Fix f =  In (f (Fix f))
data Value m where
  Nat     :: Int -> Value m
  Closure :: Monad m => [Value m] -> m (Value m) -> Value m

class Monad m => CBVMonad m where
  env  :: m [Value m]
  with :: [Value m] -> m (Value m) -> m (Value m)

fold :: Functor t => (t a -> a) -> Fix t -> a
fold f (In t) = f (fmap (fold f) t)

eval :: Eval t m => Fix t -> m (Value m)
eval = undefined -- fold evalAlgebra

pretty :: Pretty t => Fix t -> Text
pretty (In t) = prettyAlgebra t

--------------------------------------------------------------------------------
--                                ALGEBRAS                                    --
--------------------------------------------------------------------------------

class (Functor t, Monad m) => Eval t m where
  evalAlgebra :: t (m Int) -> m (Value m)

class Functor t => Pretty t where
  prettyAlgebra :: Pretty t' => t (Fix t') -> Text

--------------------------------------------------------------------------------
--                              CONSTRUCTORS                                  --
--------------------------------------------------------------------------------

data (f :+: g) t =  Inl (f t) | Inr (g t)
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl t) = Inl (fmap f t)
  fmap f (Inr t) = Inr (fmap f t)

instance (Eval f m, Eval g m) => Eval (f :+: g) m where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

instance (Pretty f, Pretty g) => Pretty (f :+: g) where
  prettyAlgebra (Inl x) = prettyAlgebra x
  prettyAlgebra (Inr y) = prettyAlgebra y

-------------------
data Natural t = Zero | Succ t

instance Functor Natural where
  fmap _ Zero     = Zero
  fmap f (Succ n) = Succ (f n)

-- instance Monad m => Eval Natural m where
--   evalAlgebra Zero     = return 0
--   evalAlgebra (Succ t) = return 0

-------------------
data Lambda t = Lam t | Index Int | App t t

instance Functor Lambda where
  fmap f (Lam t)     = Lam (f t)
  fmap _ (Index i)   = Index i
  fmap f (App t1 t2) = App (f t1) (f t2)

instance CBVMonad m => Eval Lambda m where
  evalAlgebra (Lam t) = undefined --env >>= \e -> return (Closure e i)

-- instance Pretty Lambda where
--   prettyAlgebra (Lam (In t)) = "Lam (" <> prettyAlgebra <> ")"


--------------------------------------------------------------------------------
--                                PROGRAMS                                    --
--------------------------------------------------------------------------------

foo0 :: Fix Natural
foo0 = In Zero     

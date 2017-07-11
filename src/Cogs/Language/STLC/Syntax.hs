{-# LANGUAGE FlexibleInstances,
             GADTs,
             MultiParamTypeClasses,
             OverloadedStrings,
             TypeOperators         #-}
module Cogs.Language.STLC.Syntax where

import Data.Text
import Data.Monoid

data Mu f =  In (f (Mu f))

fold :: Functor f => (f a -> a) -> Mu f -> a
fold f (In t) = f (fmap (fold f) t)


data Value m where
  Nat     :: Int -> Value m
  Closure :: Monad m => [Value m] -> m (Value m) -> Value m

-- call-by-value
class Monad m => CBVMonad m where
  envCBV  :: m [Value m]
  withCBV :: [Value m] -> m (Value m) -> m (Value m)

-- call-by-name
class Monad m => CBNMonad m where
  envCBN  :: m [m (Value m)]
  withCBN :: [m (Value m)] -> m (Value m) -> m (Value m)

eval :: Eval t m => Mu t -> m (Value m)
eval = fold evalAlgebra

pretty :: Pretty t => Mu t -> Text
pretty (In t) = prettyAlgebra t

--------------------------------------------------------------------------------
--                                ALGEBRAS                                    --
--------------------------------------------------------------------------------

class (Functor f, Monad m) => Eval f m where
  evalAlgebra :: f (m (Value m)) -> m (Value m)

class Functor t => Pretty t where
  prettyAlgebra :: Pretty t' => t (Mu t') -> Text

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

instance Pretty Natural where
  prettyAlgebra Zero = "zero"
  prettyAlgebra (Succ (In t)) = "succ (" <> prettyAlgebra t <> ")"

instance Monad m => Eval Natural m where
  evalAlgebra Zero     = return (Nat 0)
  evalAlgebra (Succ t) = do { (Nat n) <- t
                            ; return (Nat (n+1)) }

-------------------
data Lambda t = Lam t | Index Int | App t t

instance Functor Lambda where
  fmap f (Lam t)     = Lam (f t)
  fmap _ (Index i)   = Index i
  fmap f (App t1 t2) = App (f t1) (f t2)

instance CBVMonad m => Eval Lambda m where
  evalAlgebra (Lam t)     = envCBV >>= \e -> return (Closure e t)
  evalAlgebra (Index i)   = envCBV >>= \e -> return (e !! i)
  evalAlgebra (App t1 t2) = do { (Closure e t1') <- t1
                               ; t2' <- t2
                               ; withCBV (t2':e) t1' }

instance Pretty Lambda where
  prettyAlgebra (Lam (In t))          = "Lam (" <> prettyAlgebra t <> ")"
  prettyAlgebra (Index i)             = "Var " <> (pack . show $ i)
  prettyAlgebra (App (In t1) (In t2)) = "App (" <> prettyAlgebra t1
                                      <> ") (" <> prettyAlgebra t2 <> ")"


--------------------------------------------------------------------------------
--                                PROGRAMS                                    --
--------------------------------------------------------------------------------

foo0 :: Mu Natural
foo0 = In Zero

foo2 :: Mu Natural
foo2 = In (Succ (In (Succ (In Zero))))

-- foo1 :: Mu (Lambda :+: Natural)
-- foo1 = In (Lam (In Zero))

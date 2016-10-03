module Language.Sequent.Untyped.Syntax.AST where

newtype Var = Var String
  deriving (Show, Eq)

newtype Label = Label String
  deriving (Show, Eq)

data Term a
  = Call Var
  | Lit a
  | Add (Term a) (Term a)
  | Mu (Command a)
  | Lambda Var (Term a)
  deriving (Show, Eq)

data Command a
  = Let (Bind a) (Command a)
  | (Term a) :||: (Kont a)
  | Jump Label [Var]
  deriving (Show, Eq)

data Kont a
  = Ret
  | KTerm (Term a) (Kont a)
  -- | Case [Alt a]
  deriving (Show, Eq)

data Bind a
  = SBind (BindPair a)
  | RecBind [BindPair a]
  deriving (Show, Eq)

data BindPair a
  = VT Var (Term a)
  | LC Label [Var] (Command a)
  deriving (Show,Eq)

module Language.Sequent.Core.Syntax.AST where

data Kind
  = Star
  | Kind :-> Kind
  deriving (Show,Eq)

newtype TyVar = TyVar String
  deriving (Show,Eq)

data Type c
  = TVar TyVar
  | Concrete c
  | Forall TyVar Kind (Type c)
  | Exists TyVar Kind (Type c)
  -- concrete type
  -- sigma?
  deriving (Show,Eq)

newtype Var = Var String
  deriving (Show,Eq)

newtype Label = Label String
  deriving (Show,Eq)

newtype Program c = Program (Bind c)
  deriving (Show,Eq)

data Bind c
  = BBindPair (BindPair c)
  | BRec [BindPair c]
  deriving (Show,Eq)

data BindPair c
  = BPBind Var (Type c) (Term c)
  --here
  deriving (Show,Eq)

data Term c
  = Lambda Var (Type c) (Term c)
  | BigLambda TyVar Kind (Term c)
  -- | TVar Var
  -- more
  deriving (Show,Eq)

data Command c
  = C (Term c) (Kont c)
  | Let (Bind c) (Command c)
  deriving (Show,Eq)

data Kont c
  = KTerm (Term c) (Kont c)
  | KType (Type c) (Kont c)
  | Case (Alt c)
  | Ret
  deriving (Show,Eq)

data Alt c
  = ACase Var (Type c) (Command c)
  -- something
  deriving (Show,Eq)

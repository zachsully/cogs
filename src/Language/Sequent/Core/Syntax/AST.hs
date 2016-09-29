module Language.Sequent.Core.Syntax.AST where

data Kind
  = Star
  | Kind :-> Kind
  deriving (Show,Eq)

newtype TyVar = TyVar String
  deriving (Show,Eq)

data Type
  = TypVar TyVar
  deriving (Show,Eq)

newtype Var = Var String
  deriving (Show,Eq)

newtype Label = Label String
  deriving (Show,Eq)

newtype Program = Program Bind
  deriving (Show,Eq)

data Bind
  = BBindPair BindPair
  | BRec [BindPair]
  deriving (Show,Eq)

data BindPair
  = BPBind Var Type Term
  --here
  deriving (Show,Eq)

data Term
  = Lambda Var Type Term
  | BigLambda TyVar Kind Term
  | TVar Var
  -- more
  deriving (Show,Eq)

data Command
  = C Term Kont
  | Let Bind Command
  deriving (Show,Eq)

data Kont
  = KTerm Term Kont
  | KType Type Kont
  | Case Alt
  | Ret
  deriving (Show,Eq)

data Alt
  = ACase Var Type Command
  -- something
  deriving (Show,Eq)

module Language.Sequent.Core.Evaluation.Interpret
  ( interpret
  ) where

import Language.Sequent.Core.Syntax.AST

interpret :: Program a -> a
interpret (Program b) = interpBind b

interpBind :: Bind a -> a
interpBind (BBindPair bp) = interpBindPair bp
interpBind (BRec bps) = undefined

interpBindPair :: BindPair a -> a
interpBindPair () = undefined

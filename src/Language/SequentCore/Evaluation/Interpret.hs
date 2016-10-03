module Language.SequentCore.Evaluation.Interpret
  ( interpret
  ) where

import Data.List.NonEmpty

import Language.SequentCore.Syntax.AST

interpret :: Program a -> a
interpret (Program b) = interpBind b

interpBind :: Bind a -> a
interpBind (BindPair (bp :| [])) = interpBindPair bp
interpBind (BindPair (_ :| _))   = undefined

interpBindPair :: BindPair a -> a
interpBindPair _ = undefined

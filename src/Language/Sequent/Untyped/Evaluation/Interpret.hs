module Language.Sequent.Untyped.Evaluation.Interpret
  ( interpret
  ) where

import Language.Sequent.Untyped.Syntax.AST

interpret :: Num a => Term a -> a
interpret (Mu c)      = interpret . interpretCommand $ c
interpret (Lit v)     = v
interpret (Add e0 e1) = interpret e0 + interpret e1

interpretCommand :: Command a -> Term a
interpretCommand (t :||: k) = interpretKont k $ t

interpretKont :: Kont a -> (Term a -> Term a)
interpretKont Ret         = id
interpretKont (KTerm t k) = let k' = interpretKont k
                            in  \t' -> k' t

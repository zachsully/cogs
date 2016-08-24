{-# LANGUAGE OverloadedStrings #-}
module Cogs.X86 where

import Data.ByteString as BS
import Prelude hiding (print)


newtype Program a = Program [X86 a]

data Register = RAX | RBX | RCX | RDX | RSI |

data X86 a = Addq a a
           | Subq a a
           | Movq a a
           deriving Show

class Print a where
  print :: a -> ByteString

instance Print a => Print (Program a) where
  print (Program instr) = mconcat $ ["\t.globl main\nmain:\n"]
                                 ++ fmap print instr
                                 ++ ["\n"]

instance Print a => Print (X86 a) where
  print (Addq a b) = mconcat ["\taddq\t",print a,",\t",print b]
  print (Subq a b) = mconcat ["\tsubq\t",print a,",\t",print b]

instance Print Register where
  print RAX = "%rax"
  print RBX = "%rbx"
  print RCX = "%rcx"

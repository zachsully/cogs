module Cogs.Instruction.X86 where

type X86   = [Block]
type Block = [X86Instr]

data Reg = RAX | RBX | RCX | RDX | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Show,Eq)     

data X86Instr
  = Pushq
  | Popq
  | Leaq
  | Callq Reg
  | Movq Reg Reg
  | Addq Reg Reg
  deriving (Show,Eq)

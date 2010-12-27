module Types (Reg(..)
             , Label
             , Shift
             , LCommand(..)
             , Command(..)
             , Program
             , ProgramWithLabels
             , Registers
             , CommandCnt
             , WmState) where 

data Reg = R0  | R1  | R2  | R3 
         | R4  | R5  | R6  | R7 
         | R8  | R9  | R10 | R11 
         | R12 | R13 | R14 | R15 
           deriving (Eq, Enum, Show)
 
type Label = String

type Shift = Int

data LCommand = Store' Int 
              | Inc'   Reg
              | Dec'   Reg
              | Not'   Reg
              | And'   Reg
              | Or'    Reg
              | Xor'   Reg
              | Xch'   Reg
              | Jz'    Shift
              | Jnz'   Shift
              | Jzl    Label
              | Jnzl   Label
              | Shr'   Shift
              | Shl'   Shift
              | Jmp'   Reg
              | Out'   Reg
              | Sleep' Reg
              | Nop' 
              | Labeled LCommand Label
                deriving (Show)

data Command = Store Int 
             | Inc   Reg
             | Dec   Reg
             | Not   Reg
             | And   Reg
             | Or    Reg
             | Xor   Reg
             | Xch   Reg
             | Jz    Shift 
             | Jnz   Shift 
             | Shr   Shift 
             | Shl   Shift 
             | Jmp   Reg
             | Out   Reg
             | Sleep Reg
             | Nop 
               deriving (Show)

type Program = [Command]
type ProgramWithLabels = [LCommand]

type Registers  = [Int]

type CommandCnt = Int

type WmState    = (Registers, CommandCnt)


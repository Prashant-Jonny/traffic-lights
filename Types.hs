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
              | Jz'    Label
              | Jnz'   Label
              | Shr'   Shift
              | Shl'   Shift
              | Jmp'   Reg
              | Out'   Reg
              | Sleep' Reg
              | Nop' 
              | Labeled LCommand Label

instance Show LCommand where
    show command = 
        case command of 
          Store' x    -> "Store " ++ (show x)
          Inc'   r    -> "Inc "   ++ (show r) 
          Dec'   r    -> "Dec "   ++ (show r) 
          Not'   r    -> "Not "   ++ (show r) 
          And'   r    -> "And "   ++ (show r) 
          Or'    r    -> "Or "    ++ (show r) 
          Xor'   r    -> "Xor "   ++ (show r) 
          Xch'   r    -> "Xch "   ++ (show r) 
          Jz'    l    -> "Jz "    ++ l 
          Jnz'   l    -> "Jnz "   ++ l
          Shr'   s    -> "Shr "   ++ (show s) 
          Shl'   s    -> "Shl "   ++ (show s) 
          Jmp'   r    -> "Jmp "   ++ (show r) 
          Out'   r    -> "Out "   ++ (show r) 
          Sleep' r    -> "Sleep " ++ (show r) 
          Nop'        -> "Nop"
          Labeled c l -> l ++ ": " ++ (show c)


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

type Registers = [Int]

type CommandCnt = Int

type WmState = (Registers, CommandCnt)


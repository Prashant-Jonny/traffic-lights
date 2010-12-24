import System.Posix.Unistd
import Data.Bits

data Command = Store Int 
             | Inc   Reg
             | Dec   Reg
             | Not   Reg
             | And   Reg
             | Or    Reg
             | Xor   Reg
             | Xch   Reg
             | Jz    Int
             | Jnz   Int
             | Shr   Int
             | Shl   Int
             | Jmp   Reg
             | Out   Reg
             | Sleep Reg
             | Nop 

data Reg = R0 | R1 | R2 | R3 |  R4 |  R5 |  R6 |  R7 |  R8 |  R9 |  R10 |  R11 |  R12 |  R13 |  R14 |  R15 
           deriving (Eq, Ord, Enum, Show)

type Program    = [Command]
type Registers  = [Int]
type CommandCnt = Int
type WmState    = (Registers, CommandCnt)

initialState :: WmState
initialState = ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 0)

run :: Program -> IO WmState
run p = run' p initialState

run' :: Program -> WmState -> IO WmState
run' p (registers, counter) = runCommand' command (registers, counter) >>= h
    where command = p !! counter
          h newState = if (halt newState) then return newState else run' p newState

halt :: WmState -> Bool
halt (registers, _) = registers !! 0 == 100

runCommand' :: Command -> WmState -> IO WmState
runCommand' (Out reg) (state, cnt)   = (putStrLn $ show (getReg state reg)) >> return (runCommand (Out reg) state, count (Out reg) (state, cnt))
runCommand' (Sleep reg) (state, cnt) = usleep ((getReg state reg) * 1000) >> return (runCommand (Sleep reg) state, count (Sleep reg) (state, cnt))
runCommand' command (state, cnt)     = return (runCommand command state, count command (state, cnt))

runCommand :: Command -> Registers -> Registers
runCommand (Store x) state = setReg state R0 x
runCommand (Inc reg) state = setReg state reg ((getReg state reg)+1)
runCommand (Dec reg) state = setReg state reg ((getReg state reg)-1)
runCommand (Not reg) state = setReg state reg (complement (getReg state reg))
runCommand (And reg) state = setReg state R0  ((getReg state R0) .&. (getReg state R0))
runCommand (Or  reg) state = setReg state R0  ((getReg state R0) .|. (getReg state R0)) 
runCommand (Xor reg) state = setReg state R0  ((getReg state R0) `xor` (getReg state R0)) 
runCommand (Xch reg) state = if reg == R0 then state else setReg (setReg state R0 regval) reg r0val
    where r0val = getReg state R0
          regval = getReg state reg
runCommand command state   = state

getReg :: Registers -> Reg -> Int
getReg registers reg = registers !! fromEnum(reg)

setReg :: Registers -> Reg -> Int -> Registers
setReg registers reg val = take r registers ++ [val] ++ drop (r+1) registers
                           where r = fromEnum reg

count :: Command -> WmState -> CommandCnt
count (Jz  shift) (state, cnt) = if (state !! 0) == 0 then cnt+shift else cnt+1
count (Jnz shift) (state, cnt) = if not $ (state !! 0) == 0 then cnt+shift else cnt+1
count (Shr shift) (state, cnt) = cnt+shift
count (Shl shift) (state, cnt) = cnt-shift
count (Jmp reg)   (state, cnt) = getReg state reg
count command     (_, cnt)     = cnt+1


add :: Int -> Int -> Program
add a b = [Store a, 
           Xch R1, 
           Store b, 
           Inc R1,
           Dec R0,
           Out R1,
           Jnz (-3),
           Out R1,
           Store 100]
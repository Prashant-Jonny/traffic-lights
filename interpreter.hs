import System.Posix.Unistd
import Data.Bits 

data Command = Store Int 
             | Inc   Reg
             | Dec   Reg
             | Not   Int
             | And   Int
             | Or    Int
             | Xor   Int
             | Xch   Int
             | Jz    Int
             | Jnz   Int
             | Shr   Int
             | Shl   Int
             | Jmp   Int
             | Out   Int
             | Sleep Int
             | Nop 

data Reg = R0 | R1 | R2 | R3 |  R4 |  R5 |  R6 |  R7 |  R8 |  R9 |  R10 |  R11 |  R12 |  R13 |  R14 |  R15
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
runCommand' (Out reg) (state, cnt) = (putStrLn $ show (state !! reg)) >> return (runCommand (Out reg) state, count (Out reg) (state, cnt))
runCommand' (Sleep millis) (state, cnt) = usleep (millis * 1000) >> return (runCommand (Sleep millis) state, count (Sleep millis) (state, cnt))
runCommand' command (state, cnt)        = return (runCommand command state, count command (state, cnt))

runCommand :: Command -> Registers -> Registers
runCommand (Store x) state = set state 0 x
runCommand (Inc reg) state = set state reg $ (state !! reg)+1
runCommand (Dec reg) state = set state reg $ (state !! reg)-1
runCommand (Not reg) state = set state 0 x where x = complement (state !! reg)
runCommand (And reg) state = set state 0 x where x = head state .&. state !! reg
runCommand (Or  reg) state = set state 0 x where x = head state .|. state !! reg
runCommand (Xor reg) state = set state 0 x where x = head state `xor` state !! reg
runCommand (Xch reg) state = if reg == 0 then state
                                    else [r] ++ (take (reg-1) . drop 1 $ state) ++ [zero] ++ drop reg state
                                        where zero = state !! 0
                                              r = state !! reg
runCommand command state   = state

set :: Registers -> Int -> Int -> Registers
set registers regNumber value = take regNumber registers ++ [value] ++ drop (regNumber+1) registers


count :: Command -> WmState -> CommandCnt
count (Jz  shift) (state, cnt) = if (state !! 0) == 0 then cnt+shift else cnt+1
count (Jnz shift) (state, cnt) = if not $ (state !! 0) == 0 then cnt+shift else cnt+1
count (Shr shift) (state, cnt) = cnt+shift
count (Shl shift) (state, cnt) = cnt-shift
count (Jmp shift) (state, cnt) = shift
count command     (_, cnt)     = cnt+1


add :: Int -> Int -> Program
add a b = [Store a, 
           Xch 1, 
           Store b, 
           Inc 1,
           Dec 0,
           Out 1,
           Sleep 100,
           Jnz (-4),
           Out 1,
           Store 100]
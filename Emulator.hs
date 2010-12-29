module Emulator (run) where

import Types
import System.IO
import System.Posix.Unistd
import Data.Bits

initialState :: WmState
initialState = ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 0)

run :: Program -> IO WmState
run p = run' p initialState
 
run' :: Program -> WmState -> IO WmState
run' program (registers, counter) = runCommand command (registers, counter) >>= h
    where command = program !! counter
          h newState = if (halt newState) then return newState else run' program newState
 
halt :: WmState -> Bool
halt (registers, _) = getReg registers R0 == 100
 
runCommand :: Command -> WmState -> IO WmState
runCommand command (state, cnt) =
    case command of 
      Out   reg -> (putStrLn $ show (getReg state reg)) >> return (update command (state, cnt))
      Sleep reg -> usleep ((getReg state reg) * 1000)   >> return (update command (state, cnt))
      otherwise -> return (update command (state, cnt))
 
update :: Command -> WmState -> WmState
update command (state, cnt) = (updateRegisters command state, updateCounter command (state, cnt))
 
updateRegisters :: Command -> Registers -> Registers
updateRegisters command state =
    case command of 
      Store x   -> setReg state R0 x                                             
      Inc reg   -> setReg state reg ((getReg state reg)+1)                       
      Dec reg   -> setReg state reg ((getReg state reg)-1)                       
      Not reg   -> setReg state reg (complement (getReg state reg))              
      And reg   -> setReg state R0  ((getReg state R0) .&. (getReg state R0))    
      Or  reg   -> setReg state R0  ((getReg state R0) .|. (getReg state R0))    
      Xor reg   -> setReg state R0  ((getReg state R0) `xor` (getReg state R0))  
      Xch reg   -> setReg (setReg state R0 regval) reg r0val
        where r0val  = getReg state R0
              regval = getReg state reg
      otherwise -> state
 
      
updateCounter :: Command -> WmState -> CommandCnt
updateCounter command (state, cnt) =
    case command of 
      Jz  shift -> if (getReg state R0) == 0 then cnt+decode(shift) else cnt+1     
      Jnz shift -> if not $ (getReg state R0) == 0 then cnt+decode(shift) else cnt+1 
      Shr shift -> cnt+decode(shift)                                              
      Shl shift -> cnt+decode(shift)                                              
      Jmp reg   -> getReg state reg                                               
      otherwise -> cnt+1
 
getReg :: Registers -> Reg -> Int
getReg registers reg = registers !! fromEnum(reg)
 
setReg :: Registers -> Reg -> Int -> Registers
setReg registers reg val = take r registers ++ [val] ++ drop (r+1) registers
    where r = fromEnum reg

decode :: Int -> Int
decode x = if (x >= 0 && x < 8) then x
           else -(x `mod` 8)


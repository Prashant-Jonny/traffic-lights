module Compiler (compile) where

import Types
import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.Word

compile :: Program -> L.ByteString
compile = L.pack . map compileCommand

compileCommand :: Command -> Word8
compileCommand command = 
    case command of 
      Store x   -> 0x00 .|. fromIntegral(x)              
      Inc reg   -> 0x10 .|. fromIntegral(fromEnum(reg))  
      Dec reg   -> 0x20 .|. fromIntegral(fromEnum(reg))  
      Not reg   -> 0x30 .|. fromIntegral(fromEnum(reg))  
      And reg   -> 0x40 .|. fromIntegral(fromEnum(reg))  
      Or  reg   -> 0x50 .|. fromIntegral(fromEnum(reg))  
      Xor reg   -> 0x60 .|. fromIntegral(fromEnum(reg))  
      Xch reg   -> 0x70 .|. fromIntegral(fromEnum(reg))  
      Jz  shift -> 0x80 .|. fromIntegral(shift)          
      Jnz shift -> 0x90 .|. fromIntegral(shift)          
      Shr shift -> 0xa0 .|. fromIntegral(shift)          
      Shl shift -> 0xb0 .|. fromIntegral(shift)          
      Jmp reg   -> 0xc0 .|. fromIntegral(fromEnum(reg))  
      Out reg   -> 0xd0 .|. fromIntegral(fromEnum(reg))  
      Sleep reg -> 0xe0 .|. fromIntegral(fromEnum(reg))  
      Nop       -> 0xff                                  

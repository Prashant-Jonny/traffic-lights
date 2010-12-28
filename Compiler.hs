module Compiler (compile, decompile) where

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


decompile :: L.ByteString -> Program
decompile = map decompileCommand . L.unpack

decompileCommand :: Word8 -> Command
decompileCommand byte = decompileCommand' command arg
    where command = shift byte (-4)
          arg = byte .&. 0x0f
                     

decompileCommand' :: Word8 -> Word8 -> Command
decompileCommand' command arg
    | command == 0  = Store (fromIntegral arg)
    | command == 1  = Inc   (toEnum (fromIntegral arg))
    | command == 2  = Dec   (toEnum (fromIntegral arg))
    | command == 3  = Not   (toEnum (fromIntegral arg))
    | command == 4  = And   (toEnum (fromIntegral arg))
    | command == 5  = Or    (toEnum (fromIntegral arg))
    | command == 6  = Xor   (toEnum (fromIntegral arg))
    | command == 7  = Xch   (toEnum (fromIntegral arg))
    | command == 8  = Jz    (fromIntegral arg)
    | command == 9  = Jnz   (fromIntegral arg)
    | command == 10 = Shr   (fromIntegral arg)
    | command == 11 = Shl   (fromIntegral arg)
    | command == 12 = Jmp   (toEnum (fromIntegral arg))
    | command == 13 = Out   (toEnum (fromIntegral arg))
    | command == 14 = Sleep (toEnum (fromIntegral arg))
    | command == 15 && arg == 15 = Nop


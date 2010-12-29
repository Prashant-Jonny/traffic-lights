module Compiler (compile, decompile) where

import Types
import Parser (encode)
import qualified Data.ByteString.Lazy as L
import Data.Bits
import Data.Word
import qualified Data.Map as Map
import qualified Data.Set as Set

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


decompile :: L.ByteString -> ProgramWithLabels
decompile bytes = with_labels (label_map program) program
    where program = map decompileCommand $ L.unpack bytes
          lpositions = label_positions program

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

with_labels :: (Map.Map Shift Label) -> Program -> ProgramWithLabels
with_labels lmap prog = foldr f [] $ zip [0..] prog
    where f (pos, command) result = (toLCommand lmap command pos):result

toLCommand :: (Map.Map Shift Label) -> Command -> Shift -> LCommand
toLCommand lmap c pos = 
    case c of 
      Store x -> wrap (Store' x)
      Inc   r -> wrap (Inc'   r) 
      Dec   r -> wrap (Dec'   r) 
      Not   r -> wrap (Not'   r) 
      And   r -> wrap (And'   r) 
      Or    r -> wrap (Or'    r) 
      Xor   r -> wrap (Xor'   r) 
      Xch   r -> wrap (Xch'   r) 
      Jz    s -> wrap (Jz'    (get_label s))
      Jnz   s -> wrap (Jnz'   (get_label s))
      Shr   s -> wrap (Shr'   s) 
      Shl   s -> wrap (Shl'   s) 
      Jmp   r -> wrap (Jmp'   r) 
      Out   r -> wrap (Out'   r) 
      Sleep r -> wrap (Sleep' r) 
      Nop     -> wrap (Nop')    
    where needLabel = Map.member pos lmap
          wrap x = if needLabel then Labeled x (lmap Map.! pos) else x
          get_label s = (lmap Map.! (pos + encode(s)))

ls :: [String]
ls = "L":ls

labels :: [String]
labels = map f $ zip ls [0..]
    where f (l, i) = l ++ (show i)

label_map prog = Map.fromList $ zip unique_label_positions labels
    where unique_label_positions = Set.toList $ Set.fromList (label_positions prog)

label_positions :: Program -> [Shift]
label_positions = foldr f [] . (zip [0..])
    where f (pos, command) result = 
              case command of 
                Jz shift  -> (pos + encode(shift)):result
                Jnz shift -> (pos + encode(shift)):result
                otherwise -> result
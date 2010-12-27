import System.IO
import System.Posix.Unistd
import Data.Bits
import Data.Char
import Data.Word
import qualified Data.ByteString.Lazy as L

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
               deriving (Show)

data Label = Label Command

data Reg = R0  | R1  | R2  | R3 
         | R4  | R5  | R6  | R7 
         | R8  | R9  | R10 | R11 
         | R12 | R13 | R14 | R15 
           deriving (Eq, Enum, Show)

type Program    = [Command]
type Registers  = [Int]
type CommandCnt = Int
type WmState    = (Registers, CommandCnt)

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
      Xch reg   -> if reg == R0 then state else setReg (setReg state R0 regval) reg r0val
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

compile2 :: [(Exc Command)] -> L.ByteString
compile2 = L.pack . map compileCommand2

compileCommand2 :: (Exc Command) -> Word8
compileCommand2 (Error s) = error s
compileCommand2 (Return c) = compileCommand2' c

compileCommand2' :: Command -> Word8
compileCommand2' (Store x)   = 0x00 .|. fromIntegral(x)
compileCommand2' (Inc reg)   = 0x10 .|. fromIntegral(fromEnum(reg)) 
compileCommand2' (Dec reg)   = 0x20 .|. fromIntegral(fromEnum(reg)) 
compileCommand2' (Not reg)   = 0x30 .|. fromIntegral(fromEnum(reg)) 
compileCommand2' (And reg)   = 0x40 .|. fromIntegral(fromEnum(reg)) 
compileCommand2' (Or  reg)   = 0x50 .|. fromIntegral(fromEnum(reg)) 
compileCommand2' (Xor reg)   = 0x60 .|. fromIntegral(fromEnum(reg)) 
compileCommand2' (Xch reg)   = 0x70 .|. fromIntegral(fromEnum(reg)) 
compileCommand2' (Jz  shift) = 0x80 .|. fromIntegral(shift) 
compileCommand2' (Jnz shift) = 0x90 .|. fromIntegral(shift) 
compileCommand2' (Shr shift) = 0xa0 .|. fromIntegral(shift) 
compileCommand2' (Shl shift) = 0xb0 .|. fromIntegral(shift) 
compileCommand2' (Jmp reg)   = 0xc0 .|. fromIntegral(fromEnum(reg)) 
compileCommand2' (Out reg)   = 0xd0 .|. fromIntegral(fromEnum(reg)) 
compileCommand2' (Sleep reg) = 0xe0 .|. fromIntegral(fromEnum(reg)) 
compileCommand2' Nop         = 0xff


compile :: Program -> [Int]
compile = map compileCommand

decode :: Int -> Int
decode x = if (x >= 0 && x < 8) then x
           else -(x `mod` 8 + 1)

compileCommand :: Command -> Int
compileCommand (Store x)   = 0x00 .|. x
compileCommand (Inc reg)   = 0x10 .|. fromEnum(reg)
compileCommand (Dec reg)   = 0x20 .|. fromEnum(reg)
compileCommand (Not reg)   = 0x30 .|. fromEnum(reg) 
compileCommand (And reg)   = 0x40 .|. fromEnum(reg) 
compileCommand (Or  reg)   = 0x50 .|. fromEnum(reg) 
compileCommand (Xor reg)   = 0x60 .|. fromEnum(reg) 
compileCommand (Xch reg)   = 0x70 .|. fromEnum(reg) 
compileCommand (Jz  shift) = 0x80 .|. shift
compileCommand (Jnz shift) = 0x90 .|. shift
compileCommand (Shr shift) = 0xa0 .|. shift
compileCommand (Shl shift) = 0xb0 .|. shift
compileCommand (Jmp reg)   = 0xc0 .|. fromEnum(reg) 
compileCommand (Out reg)   = 0xd0 .|. fromEnum(reg) 
compileCommand (Sleep reg) = 0xe0 .|. fromEnum(reg) 
compileCommand Nop         = 0xff

add :: Int -> Int -> Program
add a b = [Store 200,
           Xch   R3,
           Store a, 
           Xch   R1, 
           Store b, 
           Inc   R1,
           Dec   R0,
           Out   R1,
           Sleep R3,
           Jnz   (11),
           Out   R1,
           Store 100]

readProgram :: IO [String]
readProgram = do eof <- isEOF 
                 if eof
                    then do hPutStrLn stdout "eof"
                            return [] 
                    else do line <- hGetLine stdin
                            lines <- readProgram
                            return (line:lines)


newtype EXC m a = MkEXC (m (Exc a))
data Exc a = Error String | Return a deriving (Show)

recover :: EXC m a -> m (Exc a)
recover (MkEXC g) = g

instance Monad m => Monad (EXC m) where
    return x = MkEXC (return (Return x))
    p >>= q  = MkEXC (recover p >>= r)
               where r (Error e)   = return (Error e)
                     r (Return x) = recover (q x)

class Transformer t where
    promote :: Monad m => m a -> t m a

instance Transformer EXC where
    promote g = MkEXC (do {x <- g; return (Return x)})



newtype Parser a = MkP (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (MkP f) s = f s

applyParser :: Parser a -> String -> a
applyParser p = fst . head . apply p

instance Monad Parser where
    return x = MkP f where f s = [(x, s)]
    p >>= q  = MkP f
               where f s = [(y, s'') | (x, s') <- apply p s, (y, s'') <- apply (q x) s']

item :: Parser Char
item = MkP f
    where f [] = []
          f (c:cs) = [(c, cs)]

zero :: Parser a
zero = MkP f where f s = []

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else zero}

char :: Char -> Parser ()
char x = do {c <- sat (== x); return ()}

string :: String -> Parser ()
string [] = return ()
string (x:xs) = do {char x; string xs; return ()}

digit :: Parser Int
digit = do {d <- sat Data.Char.isDigit; return (ord d - ord '0')}

orelse :: Parser a -> Parser a -> Parser a
p `orelse` q = MkP f
               where f s = if null ps then apply q s else ps
                           where ps = apply p s
                                      
many :: Parser a -> Parser [a]
many p = do {x <- p; xs <- many p; return (x:xs)} `orelse` return []

some :: Parser a -> Parser [a]
some p = do {x <- p; xs <- many p; return (x:xs)}

spaces :: Parser ()
spaces = many (sat isSpace) >> return ()

nat :: Int -> Parser Int
nat max = do ds <- some digit
             let m `op` n = 10 * m + n
                 d = foldl1 op ds
                 valid = d <= max
             if valid then return d else error ""

register :: Parser Reg
register = do {char 'R'; r <- nat 15; return (toEnum r)}

intError :: String -> Int -> Exc a
intError command arg = Error ("Wrong argument of command '" ++ command ++ "' - " ++ (show arg))

store  = do {string "Store"; spaces; val <- nat 15;   spaces; return (Return (Store val))}
inc    = do {string "Inc";   spaces; reg <- register; spaces; return (Return (Inc reg))}
dec    = do {string "Dec";   spaces; reg <- register; spaces; return (Return (Dec reg))}
_not   = do {string "Not";   spaces; reg <- register; spaces; return (Return (Not reg))}
_and   = do {string "And";   spaces; reg <- register; spaces; return (Return (And reg))}
_or    = do {string "Or";    spaces; reg <- register; spaces; return (Return (Or reg))}
_xor   = do {string "Xor";   spaces; reg <- register; spaces; return (Return (Xor reg))}
xch    = do {string "Xch";   spaces; reg <- register; spaces; return (Return (Xch reg))}
jz     = do {string "Jz";    spaces; val <- nat 15;   spaces; return (Return (Jz val))}
jnz    = do {string "Jnz";   spaces; val <- nat 15;   spaces; return (Return (Jnz val))}
shr    = do {string "Shr";   spaces; val <- nat 15;   spaces; return (Return (Shr val))}
shl    = do {string "Shl";   spaces; val <- nat 15;   spaces; return (Return (Shl val))}
jmp    = do {string "Jmp";   spaces; reg <- register; spaces; return (Return (Jmp reg))}
out    = do {string "Out";   spaces; reg <- register; spaces; return (Return (Out reg))}
_sleep = do {string "Sleep"; spaces; reg <- register; spaces; return (Return (Sleep reg))}
nop    = do {string "Nop";   spaces; return (Return Nop)}

isValidInt :: Int -> Bool
isValidInt i = i >= 0 && i < 16

command :: Parser (Exc Command)
command = store  `orelse`
          inc    `orelse`
          dec    `orelse`
          _not   `orelse`
          _and   `orelse`
          _or    `orelse`
          _xor   `orelse`
          xch    `orelse`
          jz     `orelse`
          jnz    `orelse`
          shr    `orelse`
          shl    `orelse`
          jmp    `orelse`
          out    `orelse`
          _sleep `orelse`
          nop

check :: Program -> [(Exc Command)]
check = undefined



program :: Parser [(Exc Command)]
program = some command

main = do text <- hGetContents stdin
          let prog   = applyParser program text
              result = compile2 prog
          L.putStr result

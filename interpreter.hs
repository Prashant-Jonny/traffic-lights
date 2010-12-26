import System.IO
import System.Posix.Unistd
import Data.Bits
import Data.Char

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
runCommand (Out reg) (state, cnt)   = (putStrLn $ show (getReg state reg)) >> return (update (Out reg) (state, cnt))
runCommand (Sleep reg) (state, cnt) = usleep ((getReg state reg) * 1000) >> return (update (Sleep reg) (state, cnt))
runCommand command (state, cnt)     = return (update command (state, cnt))

update :: Command -> WmState -> WmState
update command (state, cnt) = (updateRegisters command state, updateCounter command (state, cnt))

updateRegisters :: Command -> Registers -> Registers
updateRegisters (Store x) state = setReg state R0 x
updateRegisters (Inc reg) state = setReg state reg ((getReg state reg)+1)
updateRegisters (Dec reg) state = setReg state reg ((getReg state reg)-1)
updateRegisters (Not reg) state = setReg state reg (complement (getReg state reg))
updateRegisters (And reg) state = setReg state R0  ((getReg state R0) .&. (getReg state R0))
updateRegisters (Or  reg) state = setReg state R0  ((getReg state R0) .|. (getReg state R0)) 
updateRegisters (Xor reg) state = setReg state R0  ((getReg state R0) `xor` (getReg state R0)) 
updateRegisters (Xch reg) state = if reg == R0 then state else setReg (setReg state R0 regval) reg r0val
    where r0val = getReg state R0
          regval = getReg state reg
updateRegisters command state   = state

updateCounter :: Command -> WmState -> CommandCnt
updateCounter (Jz  shift) (state, cnt) = if (getReg state R0) == 0 then cnt+decode(shift) else cnt+1
updateCounter (Jnz shift) (state, cnt) = if not $ (getReg state R0) == 0 then cnt+decode(shift) else cnt+1
updateCounter (Shr shift) (state, cnt) = cnt+decode(shift)
updateCounter (Shl shift) (state, cnt) = cnt+decode(shift)
updateCounter (Jmp reg)   (state, cnt) = getReg state reg
updateCounter command     (_, cnt)     = cnt+1

getReg :: Registers -> Reg -> Int
getReg registers reg = registers !! fromEnum(reg)

setReg :: Registers -> Reg -> Int -> Registers
setReg registers reg val = take r registers ++ [val] ++ drop (r+1) registers
                           where r = fromEnum reg

compile :: Program -> [Int]
compile = map compileCommand

decode :: Int -> Int
decode x = if (x >= 0 && x < 8) then x
           else -(x `mod` 8 + 1)

compileCommand :: Command -> Int
compileCommand (Store x)   =  0 .|. x
compileCommand (Inc reg)   =  1 .|. fromEnum(reg)
compileCommand (Dec reg)   =  2 .|. fromEnum(reg)
compileCommand (Not reg)   =  3 .|. fromEnum(reg) 
compileCommand (And reg)   =  4 .|. fromEnum(reg) 
compileCommand (Or  reg)   =  5 .|. fromEnum(reg) 
compileCommand (Xor reg)   =  6 .|. fromEnum(reg) 
compileCommand (Xch reg)   =  7 .|. fromEnum(reg) 
compileCommand (Jz  shift) =  8 .|. shift
compileCommand (Jnz shift) =  9 .|. shift
compileCommand (Shr shift) = 10 .|. shift
compileCommand (Shl shift) = 11 .|. shift
compileCommand (Jmp reg)   = 12 .|. fromEnum(reg) 
compileCommand (Out reg)   = 13 .|. fromEnum(reg) 
compileCommand (Sleep reg) = 14 .|. fromEnum(reg) 
compileCommand Nop         = 256

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

parse :: [String] -> [Command]
parse lines = add 1 2


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

nat :: Parser Int
nat = do {ds <- some digit; return (foldl1 op ds)}
    where m `op` n = 10 * m + n

register :: Parser Reg
register = do {char 'R'; r <- nat; return (toEnum r)}

store  = do {string "Store"; spaces; val <- nat;      spaces; return (Store val)}
inc    = do {string "Inc";   spaces; reg <- register; spaces; return (Inc reg)}
dec    = do {string "Dec";   spaces; reg <- register; spaces; return (Dec reg)}
_not   = do {string "Not";   spaces; reg <- register; spaces; return (Not reg)}
_and   = do {string "And";   spaces; reg <- register; spaces; return (And reg)}
_or    = do {string "Or";    spaces; reg <- register; spaces; return (Or reg)}
_xor   = do {string "Xor";   spaces; reg <- register; spaces; return (Xor reg)}
xch    = do {string "Xch";   spaces; reg <- register; spaces; return (Xch reg)}
jz     = do {string "Jz";    spaces; val <- nat;      spaces; return (Jz val)}
jnz    = do {string "Jnz";   spaces; val <- nat;      spaces; return (Jnz val)}
shr    = do {string "Shr";   spaces; val <- nat;      spaces; return (Shr val)}
shl    = do {string "Shl";   spaces; val <- nat;      spaces; return (Shl val)}
jmp    = do {string "Jmp";   spaces; reg <- register; spaces; return (Jmp reg)}
out    = do {string "Out";   spaces; reg <- register; spaces; return (Out reg)}
_sleep = do {string "Sleep"; spaces; reg <- register; spaces; return (Sleep reg)}
nop    = do {string "Nop";   spaces; return Nop}

command :: Parser Command
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

program :: Parser Program
program = some command


import System.IO
import System.Posix.Unistd
import Data.Bits
import Data.Char
import Data.Word
import qualified Data.ByteString.Lazy as L
import List
import Types
import Emulator

compile :: [(Exc Command)] -> L.ByteString
compile = L.pack . map compileCommand

compileCommand :: (Exc Command) -> Word8
compileCommand (Error s) = error s
compileCommand (Return c) = compileCommand' c

compileCommand' :: Command -> Word8
compileCommand' command = 
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

encode :: Int -> Int
encode x = if (x >= 0 && x < 8) then x
           else 7 - x

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
             if valid then return d else error $ "Wrong digit: " ++ (show d)

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

unlabel :: ProgramWithLabels -> Program
unlabel prog = map (unlabel_command lps) $ zip prog [0..(length prog)]
    where lps = labelPositions prog

unlabel_command :: [(Label, Shift)] -> (LCommand, Shift) -> Command
unlabel_command labelPositions lcommand =
    case lcommand of
      ((Store' x), _) -> Store x 
      ((Inc'   r), _) -> Inc r   
      ((Dec'   r), _) -> Dec r   
      ((Not'   r), _) -> Not r   
      ((And'   r), _) -> And r   
      ((Or'    r), _) -> Or  r   
      ((Xor'   r), _) -> Xor r   
      ((Xch'   r), _) -> Xch r   
      ((Jz'    s), _) -> Jz  s
      ((Jnz'   s), _) -> Jnz s   
      ((Jzl    l), s) -> Jz  (label2shift labelPositions s l)
      ((Jnzl   l), s) -> Jnz (label2shift labelPositions s l)
      ((Shr'   s), _) -> Shr s
      ((Shl'   s), _) -> Shl s
      ((Jmp'   r), _) -> Jmp r    
      ((Out'   r), _) -> Out r    
      ((Sleep' r), _) -> Sleep r  
      (Nop',     _) -> Nop      
      ((Labeled lcommand label), s) -> unlabel_command labelPositions (lcommand, s)


labelPositions :: ProgramWithLabels -> [(Label, Shift)]
labelPositions prog = map l . filter labels $ zip prog [0..(length prog)] 
    where labels (command, _) = case command of
                                  Labeled _ _ -> True
                                  otherwise -> False
          l ((Labeled c l), pos) = (l, pos)

label2shift :: [(Label, Shift)] -> Shift -> Label -> Shift
label2shift lps s label = case p of 
                            Just smth -> encode((snd smth) - s)
                            otherwise -> error "Label not found"
    where pred (l, pos) = l == label
          p = find pred lps

add2 :: Int -> Int -> ProgramWithLabels
add2 a b = [Store' a, 
            Xch'   R1, 
            Store' b, 
            Labeled (Inc' R1) "Start",
            Dec'   R0,
            Jnzl  "Start",
            Out'   R1,
            Store' 100]

main = do text <- hGetContents stdin
          let prog   = applyParser program text
              result = compile prog
          L.putStr result

module Parser (parse, encode) where

import Types
import Data.Char
import Data.List
import qualified Text.ParserCombinators.Parsec as P

parse :: String -> Program
parse = unlabel . parse'


parse' :: String -> ProgramWithLabels
parse' input = case res of
                 Left e -> error (show e)
                 Right x -> x
    where res = P.parse program "(unknown)" input


unlabel :: ProgramWithLabels -> Program
unlabel prog = map (unlabel_command lps) $ zip prog [0..(length prog)]
    where lps = labelPositions prog

unlabel_command :: [(Label, Shift)] -> (LCommand, Shift) -> Command
unlabel_command labelPositions lcommand =
    case lcommand of
      ((Store' x), _) -> if (validHex x) then Store x
                        else error ("Argument '" ++ (show x) ++ "' is too big in 'Store " ++ (show x) ++ "'")
      ((Inc'   r), _) -> Inc r   
      ((Dec'   r), _) -> Dec r   
      ((Not'   r), _) -> Not r   
      ((And'   r), _) -> And r   
      ((Or'    r), _) -> Or  r   
      ((Xor'   r), _) -> Xor r   
      ((Xch'   r), _) -> Xch r   
      ((Jz'    l), s) -> case shift of 
                          Right s -> Jz s
                          Left e -> error (e ++ " in 'Jz " ++ l ++ "'")
                        where shift = (label2shift labelPositions s l)
      ((Jnz'   l), s) -> case shift of 
                          Right s -> Jnz s
                          Left e -> error (e ++ " in 'Jnz " ++ l ++ "'")
                        where shift = (label2shift labelPositions s l)
      ((Shr'   s), _) -> Shr s
      ((Shl'   s), _) -> Shl s
      ((Jmp'   r), _) -> Jmp r    
      ((Out'   r), _) -> Out r    
      ((Sleep' r), _) -> Sleep r  
      (Nop',       _) -> Nop      
      ((Labeled lcommand label), s) -> unlabel_command labelPositions (lcommand, s)


labelPositions :: ProgramWithLabels -> [(Label, Shift)]
labelPositions prog = map l . filter labels $ zip prog [0..(length prog)] 
    where labels (command, _) = case command of
                                  Labeled _ _ -> True
                                  otherwise -> False
          l ((Labeled c l), pos) = (l, pos)

label2shift :: [(Label, Shift)] -> Shift -> Label -> Either String Shift
label2shift label_positions command_pos label = 
    case label_and_pos of 
      Just smth -> if (validHex (encode relative_shift)) then (Right (encode relative_shift))
                  else (Left ("Shift '" ++ (show $ encode relative_shift) ++ "' is too big"))
                      where relative_shift = (snd smth) - command_pos
      otherwise -> (Left ("Label '" ++ label ++ "' not found"))
    where pred = (label ==) . fst 
          label_and_pos = find pred label_positions

encode :: Int -> Int
encode x = if (x >= 0 && x < 8) then x
           else 8 - x

-- Parsing:

dig :: P.GenParser Char st Int
dig = do {d <- P.digit; return (ord d - ord '0')}

nat :: P.GenParser Char st Int
nat = do ds <- P.many1 dig
         return (foldl1 op ds)
             where m `op` n = 10 * m + n
                                  
register :: P.GenParser Char st Reg
register = P.try(do {P.char 'R'; 
                     r <- nat; 
                     if validHex r then return (toEnum r)
                     else error ("Wrong register R" ++ (show r))}) P.<|>
           do {val <- P.many (P.noneOf " \n\r\t");
               error ("Wrong register '" ++ val ++ "'")}      
              
color :: P.GenParser Char st Int
color = do s <- P.many1 (P.noneOf " \n\r\t")
           case s of
             "GREEN"  -> return 4
             "YELLOW" -> return 2
             "RED"    -> return 8
             otherwise -> error ("Wrong color '" ++ s ++ "'")

validHex :: Int -> Bool
validHex d = 0 <= d && d < 16

spaces = P.many P.space
spaces1 = P.many1 P.space

store  = do {P.string "Store"; spaces1; val <- nat P.<|> color; spaces; return (Store' val)}
inc    = do {P.string "Inc";   spaces1; reg <- register; spaces; return (Inc' reg)}
dec    = do {P.string "Dec";   spaces1; reg <- register; spaces; return (Dec' reg)}
_not   = do {P.string "Not";   spaces1; reg <- register; spaces; return (Not' reg)}
_and   = do {P.string "And";   spaces1; reg <- register; spaces; return (And' reg)}
_or    = do {P.string "Or";    spaces1; reg <- register; spaces; return (Or' reg)}
_xor   = do {P.string "Xor";   spaces1; reg <- register; spaces; return (Xor' reg)}
xch    = do {P.string "Xch";   spaces1; reg <- register; spaces; return (Xch' reg)}
jz     = do {P.string "Jz";    spaces1; val <- P.many (P.noneOf " \n\r\t"); spaces; return (Jz' val)}
jnz    = do {P.string "Jnz";   spaces1; val <- P.many (P.noneOf " \n\r\t"); spaces; return (Jnz' val)}
shr    = do {P.string "Shr";   spaces1; val <- nat;      spaces; return (Shr' val)}
shl    = do {P.string "Shl";   spaces1; val <- nat;      spaces; return (Shl' val)}
jmp    = do {P.string "Jmp";   spaces1; reg <- register; spaces; return (Jmp' reg)}
out    = do {P.string "Out";   spaces1; reg <- register; spaces; return (Out' reg)}
_sleep = do {P.string "Sleep"; spaces1; reg <- register; spaces; return (Sleep' reg)}
nop    = do {P.string "Nop";   spaces1; return Nop'}
label  = do {l <- P.many (P.noneOf " \n\r\t:"); P.char ':'; spaces1; c <- command; return (Labeled c l)}


command :: P.GenParser Char st LCommand
command = P.try(label)  P.<|>
          P.try(store)  P.<|>
          P.try(inc)    P.<|> 
          P.try(dec)    P.<|> 
          P.try(_not)   P.<|> 
          P.try(_and)   P.<|> 
          P.try(_or)    P.<|> 
          P.try(_xor)   P.<|> 
          P.try(xch)    P.<|> 
          P.try(jz)    P.<|> 
          P.try(jnz)   P.<|> 
          P.try(shr)    P.<|> 
          P.try(shl)    P.<|> 
          P.try(jmp)    P.<|> 
          P.try(out)    P.<|> 
          P.try(_sleep) P.<|> 
          nop


program :: P.GenParser Char st ProgramWithLabels
program = P.many command
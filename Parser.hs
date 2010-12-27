module Parser (parse) where

import Types
import Data.Char
import qualified Text.ParserCombinators.Parsec as P

parse :: String -> Program
parse = unlabel . parse'


parse' :: String -> ProgramWithLabels
parse' input = case res of
                 Left e -> error (show e)
                 Right x -> x
    where res = P.parse program "(unknown)" input


unlabel :: ProgramWithLabels -> Program
unlabel = undefined


dig :: P.GenParser Char st Int
dig = do {d <- P.digit; return (ord d - ord '0')}

nat :: P.GenParser Char st Int
nat = do ds <- P.many1 dig
         return (foldl1 op ds)
             where m `op` n = 10 * m + n
                                  

register :: P.GenParser Char st Reg
register = do {P.char 'R'; r <- nat; return (toEnum r)}

spaces = P.many1 P.space

store  = do {P.string "Store"; P.spaces; val <- nat;      spaces; return (Store' val)}
inc    = do {P.string "Inc";   P.spaces; reg <- register; spaces; return (Inc' reg)}
dec    = do {P.string "Dec";   P.spaces; reg <- register; spaces; return (Dec' reg)}
_not   = do {P.string "Not";   P.spaces; reg <- register; spaces; return (Not' reg)}
_and   = do {P.string "And";   P.spaces; reg <- register; spaces; return (And' reg)}
_or    = do {P.string "Or";    P.spaces; reg <- register; spaces; return (Or' reg)}
_xor   = do {P.string "Xor";   P.spaces; reg <- register; spaces; return (Xor' reg)}
xch    = do {P.string "Xch";   P.spaces; reg <- register; spaces; return (Xch' reg)}
jz     = do {P.string "Jz";    P.spaces; val <- nat;      spaces; return (Jz' val)}
jnz    = do {P.string "Jnz";   P.spaces; val <- nat;      spaces; return (Jnz' val)}
jzl    = do {P.string "Jz";    P.spaces; val <- P.many (P.noneOf " \n\r\t"); spaces; return (Jzl val)}
jnzl   = do {P.string "Jnz";   P.spaces; val <- P.many (P.noneOf " \n\r\t"); spaces; return (Jnzl val)}
shr    = do {P.string "Shr";   P.spaces; val <- nat;      spaces; return (Shr' val)}
shl    = do {P.string "Shl";   P.spaces; val <- nat;      spaces; return (Shl' val)}
jmp    = do {P.string "Jmp";   P.spaces; reg <- register; spaces; return (Jmp' reg)}
out    = do {P.string "Out";   P.spaces; reg <- register; spaces; return (Out' reg)}
_sleep = do {P.string "Sleep"; P.spaces; reg <- register; spaces; return (Sleep' reg)}
nop    = do {P.string "Nop";   P.spaces; return Nop'}
label  = do {l <- P.many (P.noneOf " \n\r\t:"); P.char ':'; P.spaces; c <- command; return (Labeled c l)}


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
          P.try(jz)     P.<|> 
          P.try(jnz)    P.<|> 
          P.try(jzl)    P.<|> 
          P.try(jnzl)   P.<|> 
          P.try(shr)    P.<|> 
          P.try(shl)    P.<|> 
          P.try(jmp)    P.<|> 
          P.try(out)    P.<|> 
          P.try(_sleep) P.<|> 
          nop


program :: P.GenParser Char st ProgramWithLabels
program = P.many command
module Main () where

import qualified Data.ByteString.Lazy as L
import System.IO (stdin, stdout, putStr)
import Types
import Compiler
import Parser

main = do bytes <- L.hGetContents stdin
          let prog = decompile bytes
          putStr $ show prog

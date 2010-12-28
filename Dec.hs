#!/usr/bin/env runghc

module Dec (main) where

import qualified Data.ByteString.Lazy as L
import System.IO (stdin, stdout, putStr)
import Types
import Compiler
import Parser

main = do bytes <- L.hGetContents stdin
          let prog = decompile bytes
          putStr $ foldr f "" prog
              where f c rest = (show c) ++ "\n" ++ rest

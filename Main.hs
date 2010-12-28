#!/usr/bin/env runghc

module Main () where

import qualified Data.ByteString.Lazy as L
import System.IO
import Types
import Compiler
import Parser

main = do text <- hGetContents stdin
          let prog   = parse text
              result = compile prog
          L.putStr result

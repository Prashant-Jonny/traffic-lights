module Compiler (compile) where

import Types
import qualified Data.ByteString.Lazy as L

compile :: Program -> L.ByteString
compile = undefined
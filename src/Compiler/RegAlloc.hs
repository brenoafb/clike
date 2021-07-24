
{-# LANGUAGE OverloadedStrings #-}

module Compiler.RegAlloc where

import Data.Syntax
import Data.Bytecode

import qualified Data.Map as M
import qualified Data.ByteString as B

import Data.List (nub)
import Data.Generics.Uniplate.Data

type SymbolTable = M.Map Ident Index        -- maps symbol to register index
type RegSet = [Index] -- indicates registers used by the function

allocateRegisters :: Function -> (SymbolTable, RegSet)
allocateRegisters (Function _ args _ body) =
  let argsNames = map snd args
      decls     = nub [v | Decl t v <- universe body]
      pairs     = zip (argsNames <> decls) [1..]
      rs        = map snd pairs
   in (M.fromList pairs, rs)


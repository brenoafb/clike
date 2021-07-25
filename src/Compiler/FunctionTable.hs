module Compiler.FunctionTable
  ( FunctionTable
  , mkFunctionTable
  ) where

import Data.Bytecode
import Data.Syntax
import qualified Data.ByteString as B
import qualified Data.Map as M

type FunctionTable = M.Map Ident Type

mkFunctionTable :: Program -> [Bytecode] -> FunctionTable
mkFunctionTable prog@(Program _ funcs) libs =
  let fs = map (\(Function fn _ ftype _) -> (fn, ftype)) funcs
      lfs = concatMap f libs
    in M.fromList $ fs <> lfs

f :: Bytecode -> [(Ident, Type)]
f (Bytecode _ bcFuncs) = map (\(name, typ, _) -> (name, typ)) bcFuncs

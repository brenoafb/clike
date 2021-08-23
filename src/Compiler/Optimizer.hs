module Compiler.Optimizer
  ( optimizeProgram
  )
  where

import Typedefs
import Data.Syntax
import Compiler.Optimizer.ConstantFold
import Compiler.Optimizer.CompileTimeEval

import Data.Generics

optimizeProgram :: Program -> Program
optimizeProgram (Program is fs) = Program is fs'
  where fs' = map optimizeFunction fs

optimizeFunction :: Function -> Function
optimizeFunction = foldConstants . everywhere (mkT evalBinOp)

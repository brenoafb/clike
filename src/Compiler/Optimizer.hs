module Compiler.Optimizer
  ( optimizeProgram
  )
  where

import Typedefs
import Data.Syntax
import Compiler.Optimizer.ConstantFold
import Compiler.Optimizer.CompileTimeEval
import Compiler.Optimizer.UnusedVariables

import Data.Generics

optimizeProgram :: Program -> Program
optimizeProgram = everywhere (mkT optimizeFunction)

optimizeFunction :: Function -> Function
optimizeFunction f =
  let next = removeUnusedVariables . foldConstants $ everywhere (mkT evalBinOp) f
   in if f == next then next
                   else optimizeFunction next

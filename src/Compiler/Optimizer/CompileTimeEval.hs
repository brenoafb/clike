module Compiler.Optimizer.CompileTimeEval where

import Typedefs
import Data.Syntax

import Data.Generics.Uniplate.Data

-- we can evaluate arithmetic expressions at compiler time
canEvalAtCompileTime :: Expr -> Bool
canEvalAtCompileTime = isArithmeticExpr

isArithmeticExpr :: Expr -> Bool
isArithmeticExpr e = all arithmeticExpr $ universe e
  where arithmeticExpr (Num _) = True
--        arithmeticExpr (Byte _) = True -- only num supported atm
--        arithmeticExpr (Ptr _) = True
        arithmeticExpr (UnOp Neg _) = True
        arithmeticExpr (BinOp op _ _)
          | op `elem` [Add, Sub, Mult, Div] = True
        arithmeticExpr _ = False

evalAtCompileTime :: Expr -> Expr
evalAtCompileTime (BinOp op e1 e2) =
  let v1 = evalAtCompileTime e1
      v2 = evalAtCompileTime e2
   in evalBinOp (BinOp op v1 v2)
evalAtCompileTime (UnOp Neg e) =
  let v = evalAtCompileTime e
   in case v of
        Num x -> Num (negate x)
        _ -> UnOp Neg e -- softerror: did not return a number
evalAtCompileTime e = e

evalBinOp :: Expr -> Expr
evalBinOp (BinOp Add  (Num x) (Num y)) = Num $ x + y
evalBinOp (BinOp Sub  (Num x) (Num y)) = Num $ x - y
evalBinOp (BinOp Mult (Num x) (Num y)) = Num $ x * y
evalBinOp (BinOp Div  (Num x) (Num y)) = Num $ x `div` y
evalBinOp op = op -- noop

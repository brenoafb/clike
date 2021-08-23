module Compiler.Optimizer.ConstantFold
  ( foldConstants
  ) where

import Typedefs
import Data.Syntax

import Data.Generics
import Data.Generics.Uniplate.Data
import Data.List (foldl', nub)

import Compiler.Optimizer.CompileTimeEval

import qualified Data.Map as M

foldConstants :: Function -> Function
foldConstants f@(Function name args typ body) =
  let constants = findConstants f
   in foldr foldConstant f constants

foldConstant :: Ident -> Function -> Function
foldConstant v f@(Function name args typ body) =
  case [Assn v' e | Assn v' e <- universe body, v == v'] of
    [Assn _ e] ->
      if canEvalAtCompileTime e
         then let r = evalAtCompileTime e
                  body' = everywhere (mkT $ replaceVar v r) body
               in Function name args typ body'
         else f
    _   -> f

findConstants :: Function -> [Ident]
findConstants (Function _ _ _ body) =
  let vars = nub [v | Decl _ v <- universe body]
      varStates = M.fromList $ zip vars (repeat 0)
      assignments = [Assn v e | Assn v e <- universe body]
      finalStates = foldl' f varStates assignments
      f vs (Assn v _) = M.adjust (+ 1) v vs
      f vs _ = vs
  in map fst . filter (\(_, c) -> c == 1) $ M.toList finalStates

replaceVar :: Ident -> Expr -> Expr -> Expr
replaceVar v e (Var v')
  | v == v' = e
replaceVar _ _ e' = e'

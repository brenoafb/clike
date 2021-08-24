{-# LANGUAGE RankNTypes #-}
module Compiler.Optimizer.UnusedVariables where

import Typedefs
import Data.Syntax

import Data.Generics
import Data.Generics.Uniplate.Data
import Data.List (foldl', nub)

removeUnusedVariables :: Function -> Function
removeUnusedVariables f =
  let unusedVariables = findUnusedVariables f
   in foldr (\v f' -> everywhere (removeVarAssn v . removeVarDecl v) f') f unusedVariables

removeVarDecl :: Ident -> GenericT
removeVarDecl v a
  | isVarDecl v a = mkT (const Noop) a
  | otherwise = gmapT (removeVarDecl v) a

removeVarAssn :: Ident -> GenericT
removeVarAssn v a
  | isVarAssn v a = mkT (const Noop) a
  | otherwise = gmapT (removeVarAssn v) a

findUnusedVariables :: Function -> [Ident]
findUnusedVariables f@(Function _ args _ body) =
  let argVars = map snd args
      bodyVars = nub [v | Decl _ v <- universe body]
  in filter (isUnusedVar f) $ argVars ++ bodyVars

isUnusedVar :: Function -> Ident -> Bool
isUnusedVar f@(Function _ _ _ body) v =
  let vars = everything (++) ([] `mkQ` varRef) f
   in v `notElem` vars

isVarDecl :: Ident -> GenericQ Bool
isVarDecl v = False `mkQ` isVarDecl' v
  where isVarDecl' :: Ident -> Stmt -> Bool
        isVarDecl' v (Decl _ v')
          | v == v' = True
        isVarDecl' _ _ = False

isVarAssn :: Ident -> GenericQ Bool
isVarAssn v = False `mkQ` isVarAssn' v
  where isVarAssn' :: Ident -> Stmt -> Bool
        isVarAssn' v (Assn v' _)
          | v == v' = True
        isVarAssn' _ _ = False


varRef :: Expr -> [Ident]
varRef (Var v) = [v]
varRef _ = []

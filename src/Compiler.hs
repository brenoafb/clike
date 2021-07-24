{-# LANGUAGE OverloadedStrings #-}

module Compiler where

import Prelude hiding (GT, EQ, LT)


import Syntax
import Bytecode
import ConstantTable
import SymbolTable

import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.List (sortOn)

data Ctx = Ctx ConstantTable SymbolTable RegSet

type Error = B.ByteString

loadDependencies :: [B.ByteString] -> IO Program
loadDependencies deps = undefined -- TODO

compile :: Program -> Either Error Bytecode
compile prog@(Program _ funcs) = do
  let funcNames = map (\(Function fn _ _ _) -> fn) funcs
      ct = mkConstantTable prog
  funcsCode <- mapM (compileFunction ct) funcs
  let bcFunctions = zip funcNames funcsCode
      bcConstants = sortOn snd $ M.toList ct
  pure $ Bytecode bcConstants bcFunctions

getVarIndex :: SymbolTable -> Ident -> Either Error Index
getVarIndex st v =
  case M.lookup v st of
    Nothing -> throwError $ "Unknown variable " <> v
    Just i  -> pure i

compileFunction :: ConstantTable -> Function -> Either Error [OP]
compileFunction ct f = do
  let (st, rs) = allocateRegisters f
      ctx = Ctx ct st rs
  genFunctionCode ctx f

genFunctionCode :: Ctx
                -> Function
                -> Either Error [OP]
genFunctionCode ctx@(Ctx ct st rs) (Function "main" vars _ body) = do
  bodyCode <- compileStmt ctx body
  pure $ bodyCode
      <> [HALT]
genFunctionCode ctx@(Ctx ct st rs) (Function _ vars retType body) = do
  let varNames = map snd vars
  indices <- mapM (getVarIndex st) varNames
  let pops = map POPR indices
  bodyCode <- compileStmt ctx body
  pure $ [POPR 0]  -- save retaddr
      <> pops
      <> [PUSHR 0] -- push return address
      <> bodyCode
      <> if retType == VoidT then [PUSHI 0, RET] else [] -- push dummy value if returns void

compileStmt :: Ctx
            -> Stmt
            -> Either Error [OP]
compileStmt ctx@(Ctx ct st rs) stmt =
  case stmt of

    Decl _ _ -> pure []

    Assn ident expr        -> do
      case M.lookup ident st of
        Nothing -> throwError $ "Error: unknown variable " <> ident
        Just index -> do
          exprCode <- compileExpr ctx expr
          pure $ exprCode <> [POPR index]

    If cond body -> do
      condCode <- compileExpr ctx cond
      bodyCode <- compileStmt ctx body
      let offset = fromIntegral $ length bodyCode
      pure $ condCode <> [BZ offset] <> bodyCode

    IfElse cond conseq alt -> do
      condCode <- compileExpr ctx cond
      conseqCode <- compileStmt ctx conseq
      altCode <- compileStmt ctx alt
      let offset1 = fromIntegral $ length conseqCode + 1
          offset2 = fromIntegral $ length altCode
      pure $  condCode
           <> [BZ offset1]
           <> conseqCode
           <> [GOTO offset2]
           <> altCode

    Block stmts -> concat <$> mapM (compileStmt ctx) stmts

    While cond body -> do
      condCode <- compileExpr ctx cond
      bodyCode <- compileStmt ctx body
      let bzOffset = fromIntegral $ length bodyCode + 1
          gotoOffset = negate . fromIntegral $ length bodyCode + length condCode + 1
      pure $  condCode
           <> [BZ bzOffset]
           <> bodyCode
           <> [GOTO gotoOffset]

    Return expr -> do
      exprCode <- compileExpr ctx expr
      pure $ exprCode <> [RET]

    ExprS expr -> compileExpr ctx expr

compileExpr :: Ctx
            -> Expr
            -> Either Error [OP]
compileExpr ctx@(Ctx ct st rs) expr =
  case expr of
    Num n -> pure [PUSHI n]
    Byte b -> pure [PUSHI $ fromIntegral b]
    Ptr p -> pure [PUSHI p]
    Var v ->
      case M.lookup v st of
        Nothing -> throwError $ "Unknown variable " <> v
        Just i -> pure [PUSHR i]
    Str s ->
      case M.lookup (StringC s) ct of
        Nothing -> throwError $ "Unknown string \"" <> s <> "\""
        Just addr -> pure [PUSHI addr]
    FunCall "service" args -> do
      argsCode <- concat <$> mapM (compileExpr ctx) (reverse args)
      pure $ argsCode
          <> [SVC]
    FunCall name args -> do
      argsCode <- concat <$> mapM (compileExpr ctx) (reverse args)
      let pushRegs = map PUSHR rs
          popRegs  = map POPR (reverse rs)
      pure $ pushRegs
          <> argsCode
          <> [CALL name]
          <> [POPR 0]  -- save return value
          <> popRegs
          <> [PUSHR 0] -- push return value
    UnOp op e1 -> do
      c1 <- compileExpr ctx e1
      pure $ c1 <> [unOpBC op]
    RelOp op e1 e2    -> do
      c1 <- compileExpr ctx e1
      c2 <- compileExpr ctx e2
      pure $ c2 <> c1 <> [relOpBC op]
    BinOp op e1 e2 -> do
      c1 <- compileExpr ctx e1
      c2 <- compileExpr ctx e2
      pure $ c2 <> c1 <> [binOpBC op]

unOpBC :: UnOp -> OP
unOpBC Neg = NEG
unOpBC Not = NOT

relOpBC :: RelOp -> OP
relOpBC Eq   = EQ
relOpBC NEq  = NEQ
relOpBC Gt   = GT
relOpBC Lt   = LT
relOpBC LtEq = LTEQ
relOpBC GtEq = GTEQ

binOpBC :: BinOp -> OP
binOpBC Add  = ADD
binOpBC Sub  = SUB
binOpBC Mult = MUL
binOpBC Div  = DIV
binOpBC And  = AND
binOpBC Or   = OR


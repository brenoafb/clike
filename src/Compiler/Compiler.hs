{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.Compiler where

import Prelude hiding (GT, EQ, LT)

import Typedefs
import Data.Syntax
import Data.Bytecode
import Compiler.ConstantTable
import Compiler.FunctionTable
import Compiler.RegAlloc

import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Binary as Bin
import Data.List (sortOn)
import Data.String (fromString)

import Utils

data LCtx = LCtx SymbolTable RegSet               -- local context
data GCtx = GCtx FunctionTable ConstantTable      -- global context

linkBytecode :: [Bytecode] -> Bytecode
linkBytecode bcs = -- TODO merge constant addresses correctly
  Bytecode (concatMap bcConstants bcs) (concatMap bcFunctions bcs)

loadBytecode :: (MonadIO m)
             => B.ByteString -> m Bytecode
loadBytecode = liftIO . Bin.decodeFile . bs2str

compile :: (MonadError Error m, MonadIO m)
        => Program -> m Bytecode
compile prog@(Program imports funcs) = do
  let importNames = map (\(Import name) -> name) imports
  libs <- mapM loadBytecode importNames
  let funcNames = map (\(Function fn fargs ftype fbody) -> fn) funcs
      funcTypes = map (\(Function fn fargs ftype fbody) -> ftype) funcs
      ft = mkFunctionTable prog libs
      ct = mkConstantTable prog -- TODO take libs into account
      gctx = GCtx ft ct
  funcCodes <- mapM (compileFunction gctx) funcs
  let bcFunctions = zip3 funcNames funcTypes funcCodes
      bcConstants = sortOn snd $ M.toList ct
      bc = Bytecode bcConstants bcFunctions
  pure $ linkBytecode $ bc : libs

getVarIndex :: (MonadError Error m) => SymbolTable -> Ident -> m Index
getVarIndex st v =
  case M.lookup v st of
    Nothing -> throwError $ "Unknown variable " <> v
    Just i  -> pure i

compileFunction :: (MonadError Error m) => GCtx -> Function -> m [OP]
compileFunction gctx f = do
  let (st, rs) = allocateRegisters f
      lctx = LCtx st rs
  genFunctionCode gctx lctx f

genFunctionCode :: (MonadError Error m)
                => GCtx
                -> LCtx
                -> Function
                -> m [OP]
genFunctionCode gctx lctx (Function "main" vars _ body) = do
  bodyCode <- compileStmt gctx lctx body
  pure $ bodyCode
      <> [HALT]
genFunctionCode gctx lctx@(LCtx st rs) (Function _ vars retType body) = do
  let varNames = map snd vars
  indices <- mapM (getVarIndex st) varNames
  let pops = map POPR indices
  bodyCode <- compileStmt gctx lctx body
  pure $ [POPR 0]  -- save retaddr
      <> pops
      <> [PUSHR 0] -- push return address
      <> bodyCode
      <> [RETV | retType == VoidT]

compileStmt :: (MonadError Error m)
            => GCtx
            -> LCtx
            -> Stmt
            -> m [OP]
compileStmt gctx@(GCtx ft ct) lctx@(LCtx st rs) stmt =
  case stmt of

    Decl _ _ -> pure []

    Assn ident expr        -> do
      case M.lookup ident st of
        Nothing -> throwError $ "Error: unknown variable " <> ident
        Just index -> do
          exprCode <- compileExpr gctx lctx expr
          pure $ exprCode <> [POPR index]

    If cond body -> do
      condCode <- compileExpr gctx lctx cond
      bodyCode <- compileStmt gctx lctx body
      let offset = fromIntegral $ length bodyCode
      pure $ condCode <> [BZ offset] <> bodyCode

    IfElse cond conseq alt -> do
      condCode <- compileExpr gctx lctx cond
      conseqCode <- compileStmt gctx lctx conseq
      altCode <- compileStmt gctx lctx alt
      let offset1 = fromIntegral $ length conseqCode + 1
          offset2 = fromIntegral $ length altCode
      pure $  condCode
           <> [BZ offset1]
           <> conseqCode
           <> [GOTO offset2]
           <> altCode

    Block stmts -> concat <$> mapM (compileStmt gctx lctx) stmts

    While cond body -> do
      condCode <- compileExpr gctx lctx cond
      bodyCode <- compileStmt gctx lctx body
      let bzOffset = fromIntegral $ length bodyCode + 1
          gotoOffset = negate . fromIntegral $ length bodyCode + length condCode + 1
      pure $  condCode
           <> [BZ bzOffset]
           <> bodyCode
           <> [GOTO gotoOffset]

    Return expr -> do
      exprCode <- compileExpr gctx lctx expr
      pure $ exprCode <> [RET]

    ExprS expr -> compileExpr gctx lctx expr

compileExpr :: (MonadError Error m)
            => GCtx
            -> LCtx
            -> Expr
            -> m [OP]
compileExpr gctx@(GCtx ft ct) lctx@(LCtx st rs) expr =
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
      argsCode <- concat <$> mapM (compileExpr gctx lctx) (reverse args)
      pure $ argsCode
          <> [SVC]
    FunCall name args -> do
      case M.lookup name ft of
        Nothing -> throwError $ "Unknown function: " <> name
        Just retType -> do
          argsCode <- concat <$> mapM (compileExpr gctx lctx) (reverse args)
          let pushRegs = map PUSHR rs
              popRegs  = map POPR (reverse rs)
          pure $ pushRegs
              <> argsCode
              <> [CALL name]
              <> [POPR 0 | retType /= VoidT]  -- save return value
              <> popRegs
              <> [PUSHR 0 | retType /= VoidT] -- restore retval
    UnOp op e1 -> do
      c1 <- compileExpr gctx lctx e1
      pure $ c1 <> [unOpBC op]
    RelOp op e1 e2    -> do
      c1 <- compileExpr gctx lctx e1
      c2 <- compileExpr gctx lctx e2
      pure $ c2 <> c1 <> [relOpBC op]
    BinOp op e1 e2 -> do
      c1 <- compileExpr gctx lctx e1
      c2 <- compileExpr gctx lctx e2
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


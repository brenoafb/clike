{-# LANGUAGE OverloadedStrings #-}

module Compiler where

import Syntax
import Bytecode

import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M
import qualified Data.ByteString as B

type ConstantTable = M.Map Constant Address
type SymbolTable = M.Map Ident Index
type RegSet = [Index] -- indicates registers used by the function

data Ctx = Ctx ConstantTable SymbolTable RegSet

type Error = B.ByteString

loadDependencies :: [B.ByteString] -> IO Program
loadDependencies = undefined

compile :: [Program] -> Bytecode
compile = undefined

compileFunction :: Ctx
                -> Function
                -> Either Error [OP]
compileFunction ctx (Function _ _ _ body) =
  compileStmt ctx body

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
          pure $ exprCode <> [STORE index]

    If cond body -> do
      condCode <- compileExpr ctx cond
      bodyCode <- compileStmt ctx body
      let offset = fromIntegral $ length bodyCode
      pure $ condCode <> [BZ offset] <> bodyCode

    IfElse cond conseq alt -> do
      condCode <- compileExpr ctx cond
      conseqCode <- compileStmt ctx conseq
      altCode <- compileStmt ctx alt
      let offset1 = fromIntegral $ length conseqCode
          offset2 = fromIntegral $ length altCode
      pure $  condCode
           <> [BZ offset1]
           <> conseqCode
           <> [BZ offset2]
           <> altCode

    Block stmts -> concat <$> mapM (compileStmt ctx) stmts

    While cond body -> do
      condCode <- compileExpr ctx cond
      bodyCode <- compileStmt ctx body
      let offset = fromIntegral $ length bodyCode
      pure $  condCode
           <> [BZ offset]
           <> bodyCode

    Return expr -> do
      exprCode <- compileExpr ctx expr
      pure $ exprCode <> [RET]

    ExprS expr -> compileExpr ctx expr

compileExpr :: Ctx
            -> Expr
            -> Either Error [OP]
compileExpr ctx expr =
  case expr of
    Num n             -> undefined
    Byte b            -> undefined
    Ptr p             -> undefined
    Var v             -> undefined
    Str s             -> undefined
    FunCall name args -> undefined
    RelOp op e1 e2    -> undefined
    UnOp op e1        -> undefined
    BinOp op e1 e2    -> undefined


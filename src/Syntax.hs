{-# LANGUAGE DeriveDataTypeable #-}

module Syntax where

import qualified Data.ByteString as B
import Data.Int
import Data.Data

data Program = Program [Import] [Function]
  deriving Show

newtype Import = Import B.ByteString
  deriving Show

data Function = Function B.ByteString [(Type, B.ByteString)] Type Stmt
  deriving Show

type Ident = B.ByteString

data Type = IntT
          | ByteT
          | PtrT
          | StrT
          | VoidT
          deriving (Eq, Show, Data, Typeable)

data Expr = Num Int32
          | Byte Int8
          | Ptr Int32
          | Var Ident
          | Str B.ByteString
          | FunCall Ident [Expr]
          | RelOp { relOp :: RelOp, e1 :: Expr, e2 :: Expr}
          | UnOp  { unOp :: UnOp, e1 :: Expr }
          | BinOp { binOp :: BinOp, e1 :: Expr, e2 :: Expr}
          deriving (Show, Data, Typeable)

data UnOp = Neg | Not
  deriving (Eq, Show, Data, Typeable)

data RelOp = Eq | NEq | Gt | Lt | LtEq | GtEq
  deriving (Eq, Show, Data, Typeable)

data BinOp = Add
           | Sub
           | Mult
           | Div
           | And
           | Or
  deriving (Eq, Show, Data, Typeable)

data Stmt = Decl Type B.ByteString
          | Assn Ident Expr
          | If Expr Stmt
          | IfElse Expr Stmt Stmt
          | Block [Stmt]
          | While Expr Stmt
          | Return Expr
          | ExprS Expr
          deriving (Show, Data, Typeable)

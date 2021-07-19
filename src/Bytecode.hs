{-# LANGUAGE OverloadedStrings #-}

module Bytecode where

import qualified Data.ByteString as B
import Data.Int

type Index = Int32
type Address = Int32

data Constant = IntC Int32
              | StringC B.ByteString
              | BoolC Int8
              deriving Show

data Bytecode = Bytecode
  { bcConstants :: [(Constant, Address)]
  , bcFunctions :: [Function]
  } deriving Show

data Function = Function
  { fName :: B.ByteString
  , fCode :: [OP]
  } deriving Show

data OP = PUSH Int32        -- push integer onto stack
        | LOAD Index        -- load register value to stack
        | STORE Index       -- store top of stack in register at index
        | GOTO Index        -- goto pc + index unconditionally
        | CALL B.ByteString -- call function
        | BZ Index          -- branch to pc + index if top of stack is zero
        | LOADPC            -- load PC + 2 onto the stack
        | STOREPC           -- store top of stack onto PC
        | HALT
        | ADD
        | SUB
        | MUL
        | DIV
        | NEG
        | AND
        | OR
        | NOT
        deriving (Eq, Show)

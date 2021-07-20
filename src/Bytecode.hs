
module Bytecode where

import qualified Data.ByteString as B
import Data.Int

type Index = Int32
type Address = Int32

data Constant = IntC Int32
              | ByteC Int8
              | StringC B.ByteString
              deriving (Eq, Ord, Show)

data Bytecode = Bytecode
  { bcConstants :: [(Constant, Address)]
  , bcFunctions :: [BCFunction]
  } deriving Show

data BCFunction = BCFunction
  { fName :: B.ByteString
  , fCode :: [OP]
  } deriving Show

data OP = PUSHI Int32        -- push integer onto stack
        | PUSHR Index        -- load register value to stack
        | POPR  Index        -- store top of stack in register at index
        | GOTO  Index        -- goto pc + index unconditionally
        | CALL  B.ByteString -- call function
        | RET                -- return from function
        | BZ Index           -- branch to pc + index if top of stack is zero
        | SVC                -- call service routine
        | LW Index           -- addr <- pop; R[index] <- mem[addr] (word)
        | LB Index           -- addr <- pop; R[index] <- mem[addr] (byte)
        | SW Index           -- addr <- pop; mem[addr] <- R[index] (word)
        | SB Index           -- addr <- pop; mem[addr] <- R[index] (byte)
        | HALT
        | ADD
        | SUB
        | MUL
        | DIV
        | NEG
        | AND
        | OR
        | NOT
        | EQ
        | NEQ
        | GT
        | LT
        | LTEQ
        | GTEQ
        deriving (Eq, Show)

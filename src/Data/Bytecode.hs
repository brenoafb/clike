{-# LANGUAGE DeriveGeneric #-}

module Data.Bytecode where

import qualified Data.ByteString as B
import Data.Int
import Data.String (fromString)
import Data.Binary
import GHC.Generics (Generic)

type Index = Int32
type Address = Int32

data Bytecode = Bytecode
  { bcConstants :: [(Constant, Address)]
  , bcFunctions :: [(B.ByteString, [OP])]
  } deriving Generic

instance Binary Bytecode

data Constant = IntC Int32
              | ByteC Int8
              | StringC B.ByteString
              deriving (Eq, Ord, Show, Generic)

instance Binary Constant


instance Show Bytecode where
  show bc = concatMap (\f -> showFunction f) (bcFunctions bc)

showFunction (name, code) =
  show name <> "\n"
  <> concatMap (\op -> "\t" <> show op <> "\n") code

data OP = PUSHI Int32        -- push integer onto stack
        | PUSHR Index        -- load register value to stack
        | POPR  Index        -- store top of stack in register at index
        | GOTO  Index        -- goto pc + index unconditionally
        | CALL  B.ByteString -- call function
        | RET                -- return from function
        | BZ Index           -- branch to pc + index if top of stack is zero
        | SVC                -- call service routine
        | RW                 -- addr <- pop; push mem[addr] (word)
        | RB                 -- addr <- pop; push mem[addr] (byte)
        | WW                 -- TODO addr <- pop; x <- pop; mem[addr] <- x (word)
        | WB                 -- TODO addr <- pop; x <- pop; mem[addr] <- x (byte)
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
        deriving (Eq, Show, Generic)

instance Binary OP

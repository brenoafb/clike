module Data.Bytecode where

import qualified Data.ByteString as B
import Data.Int
import Data.String (fromString)

type Index = Int32
type Address = Int32

data Constant = IntC Int32
              | ByteC Int8
              | StringC B.ByteString
              deriving (Eq, Ord, Show)

data Bytecode = Bytecode
  { bcConstants :: [(Constant, Address)]
  , bcFunctions :: [(B.ByteString, [OP])]
  }

instance Show Bytecode where
  show bc = concatMap (\f -> show f) (bcFunctions bc)

data BCFunction = BCFunction
  { fName :: B.ByteString
  , fCode :: [OP]
  }

instance Show BCFunction where
  show f =
    show (fName f) <> "\n"
    <> concatMap (\op -> "\t" <> show op <> "\n") (fCode f)

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
        deriving (Eq, Show)

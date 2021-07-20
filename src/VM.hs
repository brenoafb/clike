{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module VM where

import Prelude hiding (EQ, GT, LT)
import Bytecode

import Data.Int
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as V

import Control.Monad.State
import Control.Monad.Except

type Error = B.ByteString
type Memory = V.Vector Int8

data VM =
  VM { pc :: Int32
     , currentFunction :: Maybe B.ByteString
     , stack :: [Int32]
     , nRegs :: Int8
     , registers :: V.Vector Int32
     , memSize :: Int32
     , memory :: Memory
     }

initVM :: Int8 -> Int32 -> [(Constant, Address)] -> VM
initVM nRegs memSize caddrs =
  let registers = V.replicate (fromIntegral nRegs) 0
      memory = initMemory memSize caddrs
   in VM { pc = 0
         , currentFunction = pure "main"
         , stack = []
         , nRegs = nRegs
         , registers = registers
         , memSize = memSize
         , memory = memory
         }

initMemory :: Int32 -> [(Constant, Address)] -> Memory
initMemory memSize caddrs = zeros `V.update` (V.fromList baddrs)
  where maddrs = map (\(c,a) -> (fromIntegral a, const2mem c)) caddrs
        unpack a m = zip [a..] m
        baddrs = concatMap (uncurry unpack) maddrs
        zeros = V.replicate (fromIntegral memSize) 0

const2mem :: Constant -> [Int8]
const2mem (IntC w)    = word2mem w
const2mem (ByteC b)   = byte2mem b
const2mem (StringC s) = str2mem  s

str2mem :: B.ByteString -> [Int8]
str2mem = map fromIntegral . B.unpack

word2mem :: Int32 -> [Int8]
word2mem w = [b1, b2, b3, b4]
  where b1 = (fromIntegral (w `shift`  0) .&. mask) :: Int8
        b2 = (fromIntegral (w `shift`  8) .&. mask) :: Int8
        b3 = (fromIntegral (w `shift` 16) .&. mask) :: Int8
        b4 = (fromIntegral (w `shift` 24) .&. mask) :: Int8
        mask = -128 -- 0xff


byte2mem :: Int8 -> [Int8]
byte2mem = pure

push :: Int32 -> VM -> VM
push x vm = vm { stack = x : stack vm }

pop :: (MonadState VM m, MonadIO m, MonadError Error m) => Maybe Index -> m Int32
pop mi = do
  vm <- get
  case stack vm of
    (x:xs) ->
      let newRegisters = case mi of
            Nothing -> registers vm
            Just i -> registers vm V.// [(fromIntegral i, x)]
       in put vm { stack = xs
                 , registers = newRegisters
                 } >> pure x
    [] -> throwError "VM error: empty stack (POP)"

goto :: Index -> VM -> VM
goto i vm = vm { pc = pc vm + i }

incPC :: (MonadState VM m, MonadIO m, MonadError Error m) => m ()
incPC = do
  vm <- get
  put vm { pc = pc vm + 1 }

stackBinOp :: (MonadState VM m, MonadIO m, MonadError Error m)
           => (Int32 -> Int32 -> Int32) -> m Int32
stackBinOp binop = do
  vm <- get
  case stack vm of
    (x:y:xs) ->
      let result = x `binop` y
          in put vm { stack = result : xs } >> pure result
    _ -> throwError "Unsufficient operands in stack"

executeOp :: (MonadState VM m, MonadIO m, MonadError Error m) => OP -> m ()

executeOp (PUSHI int32) = modify (push int32) >> incPC

executeOp (PUSHR index) = do
  vm <- get
  let i = fromIntegral index
  case registers vm V.!? i of
    Nothing -> throwError "Invalid register index"
    Just v -> modify (push v) >> incPC

executeOp (POPR  index) = pop (pure index) >> pure ()

executeOp (GOTO  index) = modify (goto index) >> incPC

executeOp (CALL  f    ) = undefined

executeOp (RET        ) = undefined

executeOp (BZ index   ) = do
  p <- pop Nothing -- + 1
  let inc = if p == 0
              then index
              else 0
   in modify (\vm -> vm { pc = pc vm + inc }) >> incPC

executeOp (SVC        ) = undefined
executeOp (LW index   ) = undefined
executeOp (LB index   ) = undefined
executeOp (SW index   ) = undefined
executeOp (SB index   ) = undefined
executeOp HALT = pure ()
executeOp ADD  = stackBinOp (+) >> incPC
executeOp SUB  = stackBinOp (-) >> incPC
executeOp MUL  = stackBinOp (*) >> incPC
executeOp DIV  = undefined
executeOp NEG  = undefined
executeOp AND  = undefined
executeOp OR   = undefined
executeOp NOT  = undefined
executeOp EQ   = undefined
executeOp NEQ  = undefined
executeOp GT   = undefined
executeOp LT   = undefined
executeOp LTEQ = undefined
executeOp GTEQ = undefined

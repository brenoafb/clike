{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module VM where

import Prelude hiding (EQ, GT, LT)
import Bytecode

import Data.Int
import Data.Bits
import Data.String (fromString)
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Vector as V

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Loops (untilM_)

type Error = B.ByteString
type Memory = V.Vector Int8
type Code = V.Vector OP
type FunctionTable = M.Map B.ByteString Code

data VM =
  VM { pc :: Int32
     , callStack :: [B.ByteString]
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
         , callStack = ["main"]
         , stack = []
         , nRegs = nRegs
         , registers = registers
         , memSize = memSize
         , memory = memory
         }

mkFunctionTable :: [(B.ByteString, [OP])] -> FunctionTable
mkFunctionTable =
  M.fromList . map (\(n, ops) -> (n, V.fromList ops))

executeVM :: ( MonadState VM m
             , MonadIO m
             , MonadError Error m
             , MonadReader FunctionTable m
             ) => m ()
executeVM = untilM_ (pure ()) cycleVM

cycleVM :: ( MonadState VM m
           , MonadIO m
           , MonadError Error m
           , MonadReader FunctionTable m
           ) => m Bool
cycleVM = do
  ft <- ask
  vm <- get
  case callStack vm of
    [] -> throwError "Empty call stack"
    (f:_) -> do
      code <- lookupFunction f
      op <- getOp code (pc vm)
      case op of
        HALT -> pure True
        _ -> executeOp op >> pure False

getOp :: (MonadError Error m) => Code -> Int32 -> m OP
getOp code pc =
  case code V.!? fromIntegral pc of
    Nothing -> throwError $ "Invalid pc " <> fromString (show pc)
    Just op -> pure op

initMemory :: Int32 -> [(Constant, Address)] -> Memory
initMemory memSize caddrs = zeros `V.update` V.fromList baddrs
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

pushCallStack :: B.ByteString -> VM -> VM
pushCallStack f vm = vm { callStack = f : callStack vm }

popCallStack :: (MonadState VM m, MonadIO m, MonadError Error m) => m B.ByteString
popCallStack = do
  stack <- gets callStack
  case stack of
    [] -> throwError "Empty call stack"
    (x:xs) -> do
      vm <- get
      put vm { callStack = xs }
      pure x

push :: Int32 -> VM -> VM
push x vm = vm { stack = x : stack vm }

pop :: (MonadState VM m, MonadError Error m) => Maybe Index -> m Int32
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


executeOp :: ( MonadState VM m
             , MonadIO m
             , MonadError Error m
             ) => OP -> m ()

executeOp (PUSHI int32) = modify (push int32) >> incPC

executeOp (PUSHR index) = do
  vm <- get
  let i = fromIntegral index
  case registers vm V.!? i of
    Nothing -> throwError "Invalid register index"
    Just v -> modify (push v) >> incPC

executeOp (POPR  index) = pop (pure index) >> incPC

executeOp (GOTO  index) = modify (goto index) >> incPC

executeOp (CALL  f) = do
  pc <- gets pc
  modify (push $ pc + 1)
  modify (pushCallStack f)

executeOp RET = do
  _ <- popCallStack
  retVal <- pop Nothing
  retAddr <- pop Nothing
  vm <- get
  put vm { pc = retAddr }
  modify ( push retVal )

executeOp (BZ index   ) = do
  p <- pop Nothing -- + 1
  let inc = if p == 0
              then index
              else 0
   in modify (\vm -> vm { pc = pc vm + inc }) >> incPC

executeOp SVC = pop Nothing >>= handleSVC >> incPC
executeOp (LW index   ) = undefined
executeOp (LB index   ) = undefined
executeOp (SW index   ) = undefined
executeOp (SB index   ) = undefined
executeOp HALT = pure ()
executeOp ADD  = stackBinOp (+) >> incPC
executeOp SUB  = stackBinOp (-) >> incPC
executeOp MUL  = stackBinOp (*) >> incPC
executeOp DIV  = stackBinOp div >> incPC
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

handleSVC :: (MonadState VM m, MonadIO m, MonadError Error m) => Int32 -> m ()
handleSVC 1 = do
  int <- pop Nothing
  liftIO $ print int

lookupFunction :: ( MonadError Error m
                  , MonadReader FunctionTable m
                  ) => B.ByteString -> m Code
lookupFunction f = do
  ft <- ask
  case M.lookup f ft of
    Nothing -> throwError $ "Unknown function " <> f
    Just c -> pure c

stackBinOp :: (MonadState VM m, MonadIO m, MonadError Error m)
           => (Int32 -> Int32 -> Int32) -> m Int32
stackBinOp binop = do
  vm <- get
  case stack vm of
    (x:y:xs) ->
      let result = x `binop` y
          in put vm { stack = result : xs } >> pure result
    _ -> throwError "Unsufficient operands in stack"

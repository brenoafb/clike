{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module VM.VM where

import Prelude hiding (EQ, GT, LT)

import Data.Bytecode
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

import Debug.Trace (trace)

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

instance Show VM where
  show vm =  "cs\t\t"      <> show (callStack vm) <> "\n"
          <> "pc\t\t"      <> show (pc vm) <> "\n"
          <> "stack\t\t"   <> show (stack vm) <> "\n"
          <> "registers\t" <> show  (registers vm) <> "\n"
          <> "memory\t"    <> show  (memory vm) <> "\n"

vmError = undefined

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
  where b1 = fromIntegral ((w `shiftR`  0) .&. mask)
        b2 = fromIntegral ((w `shiftR`  8) .&. mask)
        b3 = fromIntegral ((w `shiftR` 16) .&. mask)
        b4 = fromIntegral ((w `shiftR` 24) .&. mask)
        mask = 0xff

mem2word :: [Int8] -> Int32
mem2word (b1:b2:b3:b4:_) =
      fromIntegral b1 `shiftL`  0
  .|. fromIntegral b2 `shiftL`  8
  .|. fromIntegral b3 `shiftL` 16
  .|. fromIntegral b4 `shiftL` 24
mem2word _ = 0 -- TODO

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
    [] -> throwError $ "VM error: empty stack (POP)\n" <> fromString (show vm)

goto :: Index -> VM -> VM
goto i vm = vm { pc = pc vm + i }

incPC :: (MonadState VM m, MonadIO m, MonadError Error m) => m ()
incPC = do
  vm <- get
  put vm { pc = pc vm + 1 }

setPC :: (MonadState VM m, MonadIO m, MonadError Error m) => Int32 -> m ()
setPC pc = do
  vm <- get
  put vm { pc = pc }


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

executeOp (GOTO  index) = modify (goto index)

executeOp (CALL  f) = do
  pc <- gets pc
  vm <- get
  modify (push $ pc + 1)
  modify (pushCallStack f)
  setPC 0
  -- vm <- get
  -- trace ("CALL " <> "\n" <> show vm) (pure ())

executeOp RET = do
  f <- popCallStack
  -- vm <- get
  -- trace ("RET from  " <> show f <> "\n" <> show vm) (pure ())
  retVal <- pop Nothing
  retAddr <- pop Nothing
  vm <- get
  setPC retAddr
  modify ( push retVal )

executeOp (BZ index   ) = do
  p <- pop Nothing -- + 1
  let inc = if p == 0
              then index
              else 0
   in modify (\vm -> vm { pc = pc vm + inc + 1 })

executeOp SVC = pop Nothing >>= handleSVC >> incPC

executeOp RW = do
  addr <- pop Nothing
  value <- readAddressWord addr
  modify $ push value
  incPC

executeOp RB = do
  addr <- pop Nothing
  value <- readAddressByte addr
  modify (push $ fromIntegral value)
  incPC

executeOp WW = do
  addr <- pop Nothing
  word <- pop Nothing
  writeAddressWord addr word
  incPC

executeOp WB = do
  addr <- pop Nothing
  byte <- pop Nothing
  writeAddressByte addr (fromIntegral byte)
  incPC

executeOp HALT = pure ()
executeOp ADD  = stackBinOp (+)   >> incPC
executeOp SUB  = stackBinOp (-)   >> incPC
executeOp MUL  = stackBinOp (*)   >> incPC
executeOp DIV  = stackBinOp div   >> incPC
executeOp AND  = stackBinOp (\x y -> if x == 1 && y == 1 then 1 else 0) >> incPC
executeOp OR   = stackBinOp (\x y -> if x == 1 || y == 1 then 1 else 0) >> incPC
executeOp EQ   = stackBinOp (\x y -> if x == y then 1 else 0) >> incPC
executeOp NEQ  = stackBinOp (\x y -> if x == y then 0 else 1) >> incPC
executeOp GT   = stackBinOp (\x y -> if x >  y then 1 else 0) >> incPC
executeOp LT   = stackBinOp (\x y -> if x <  y then 1 else 0) >> incPC
executeOp GTEQ = stackBinOp (\x y -> if x >= y then 1 else 0) >> incPC
executeOp LTEQ = stackBinOp (\x y -> if x <= y then 1 else 0) >> incPC
executeOp NEG  = stackUnOp negate >> incPC
executeOp NOT  = stackUnOp  (\x -> if x == 0 then 1 else 0) >> incPC

readAddressByte :: (MonadState VM m, MonadError Error m) => Int32 -> m Int8
readAddressByte addr = do
  vm <- get
  case memory vm V.!? fromIntegral addr of
    Nothing -> throwError $ "Invalid address: " <> fromString (show addr)
    Just b  -> pure b

readAddressWord :: (MonadState VM m, MonadError Error m) => Int32 -> m Int32
readAddressWord addr = do
  -- TODO addr should be word-aligned (multiple of 4)
  vm <- get
  let mem = memory vm
      addr' = fromIntegral addr
  if V.length mem <= addr' + 3
     then throwError $ "Invalid word address " <> fromString (show addr)
     else
       let b1 = mem V.! addr'
           b2 = mem V.! addr' + 1
           b3 = mem V.! addr' + 2
           b4 = mem V.! addr' + 3
        in pure $ mem2word [b1,b2,b3,b4]

writeAddressByte :: (MonadState VM m, MonadError Error m) => Int32 -> Int8 -> m ()
writeAddressByte addr byte = do
  vm <- get
  let addr' = fromIntegral addr
      mem   = memory vm
  if V.length mem <= addr'
     then throwError $ "Invalid address: " <> fromString (show addr)
     else do
       let mem' = mem V.// [(addr', byte)]
           vm'  = vm { memory = mem' }
       put vm'

writeAddressWord :: (MonadState VM m, MonadError Error m) => Int32 -> Int32 -> m ()
writeAddressWord addr word = do
  vm <- get
  let addr' = fromIntegral addr
      mem   = memory vm
  if V.length mem <= addr' + 3
     then throwError $ "Invalid address: " <> fromString (show addr)
     else do
       let bytes = word2mem word
           mem' = mem V.// zip [addr'..] bytes
           vm'  = vm { memory = mem' }
       put vm'

handleSVC :: (MonadState VM m, MonadIO m, MonadError Error m) => Int32 -> m ()
handleSVC 1 = do           -- print int
  int <- pop Nothing
  liftIO $ print int

handleSVC 2 = do           -- print byte
  word <- pop Nothing
  let byte = fromIntegral word :: Int8
  liftIO $ print byte

handleSVC 3 = do           -- print char
  word <- pop Nothing
  let byte = toEnum (fromIntegral word) :: Char
  liftIO $ print byte

handleSVC 11 = do           -- read int from memory
  addr <- pop Nothing
  word <- readAddressWord addr
  modify (push word)

handleSVC 12 = do           -- read byte from memory
  addr <- pop Nothing
  byte <- readAddressByte addr
  modify (push $ fromIntegral byte)

handleSVC 13 = do           -- write word to memory
  addr <- pop Nothing
  word <- pop Nothing
  writeAddressWord addr word

handleSVC 14 = do           -- write byte to memory
  addr <- pop Nothing
  byte <- fromIntegral <$> pop Nothing
  writeAddressByte addr byte

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

stackUnOp :: (MonadState VM m, MonadIO m, MonadError Error m)
           => (Int32 -> Int32) -> m Int32
stackUnOp unop = do
  vm <- get
  case stack vm of
    (x:xs) ->
      let result = unop x
          in put vm { stack = result : xs } >> pure result
    _ -> throwError "Unsufficient operands in stack"

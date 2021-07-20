module VM where

import Bytecode

import Data.Int
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as V

type Memory = V.Vector Int8

data VM =
  VM { pc :: Int32
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

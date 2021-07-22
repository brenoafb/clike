{-# LANGUAGE OverloadedStrings #-}

module ConstantTable where

import Syntax
import Bytecode

import qualified Data.Map as M
import qualified Data.ByteString as B

import Data.List (nub)
import Data.Generics.Uniplate.Data

type ConstantTable = M.Map Constant Address -- maps constant to address in memory

constLength :: Constant -> Int
constLength (StringC s) = B.length s
constLength (IntC _) = 8
constLength (ByteC _) = 1

mkConstantTable :: Program -> ConstantTable
mkConstantTable p = M.fromList $ zip constants addrs
  where addrs = constantsToAddrs constants
        constants = getProgramConstants p

constantsToAddrs :: [Constant] -> [Address]
constantsToAddrs [] = []
constantsToAddrs (c:cs) = map (fromIntegral . snd) f
  where f = scanr (\x1 (x0, curr) -> (x1, 1 + curr + constLength x0)) (c, 0) cs

getProgramConstants :: Program -> [Constant]
getProgramConstants (Program _ fs) =
  nub $ concatMap getFunctionConstants fs

getFunctionConstants :: Function -> [Constant]
getFunctionConstants (Function _ _ _ body) =
  nub [StringC s | Assn v (Str s) <- universe body] -- currently only strings are stored as constants

mkFunctionConstantList:: Function -> [(Constant, Address)]
mkFunctionConstantList (Function _ _ _ body) =
  let constants = nub [s | Assn v (Str s) <- universe body] -- currently only strings are stored as constants
      addrs = scanr (\c acc -> B.length c + 1) 0 constants
   in zip (map StringC constants) (map fromIntegral addrs)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Translator.Translator where

import Prelude hiding (GT, EQ, LT)

import Typedefs
import Data.CSyntax
import Data.Bytecode
import Data.Int
import Data.String (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import Control.Monad.Except
import Control.Monad.State

type TCtx = V.Vector Ident

toByteString = fromString . show

mkTCtx :: BCFunction -> TCtx
mkTCtx (name, _, ops) = snd $ execState (s indexedOps) (0, V.replicate n "")
  where n = length ops
        indexedOps :: [(Index, OP)]
        indexedOps = zip ([0..] :: [Index]) ops
        s :: [(Index, OP)] -> State (Int, TCtx) ()
        s [] = pure ()
        s ((i, op):ops) =
          case op of
            GOTO d -> do
              (c, _) <- get
              let label = name <> "_BRANCH_" <> fromString (show c)
                  idx   = fromIntegral $ i + d
              modify (\(c, v) -> (c + 1, v V.// [(idx, label)]))
              s ops
            BZ d   -> do
              (c, _) <- get
              let label = name <> "_BRANCH_" <> fromString (show c)
                  idx   = fromIntegral $ i + d
              modify (\(c, v) -> (c + 1, v V.// [(idx, label)]))
              s ops
            _      -> s ops

translateBytecode :: (MonadError Error m) => Bytecode -> m CProgram
translateBytecode (Bytecode constants functions) = do -- TODO handle constants
  cFunctions <- mapM translateFunction functions
  let cIncludes =
        [ CLibInclude "stdio.h"
        ]
      cDefines =
        [ CDefine "MEMSIZE" "4096"
        , CDefine "STACKSIZE" "4096"
        , CDefine "NREGS" "32"
        , CDefine "DUMMY" "0"
        ]
      cGlobals =
        [ CDecl CInt "e1"
        , CDecl CInt "e2"
        , CDecl CInt "retval"
        , CInit CInt "sp" "0"
        , CArrDecl CUChar "mem" "MEMSIZE"
        , CArrDecl CUChar "stack" "STACKSIZE"
        , CArrDecl CUChar "registers" "NREGS"
        ]
      cText = serviceFuncText
  pure $ CProgram cIncludes cDefines cGlobals serviceFuncText cFunctions

translateFunction :: (MonadError Error m) => BCFunction -> m CFunction
translateFunction f@(name, rettype, ops) = do
  let indexedOps = zip [0..] ops
  let tctx = mkTCtx f
  stmts <- concat <$> mapM (uncurry $ translateOp tctx) indexedOps
  let cRet = if name == "main" then CInt else CVoid
  pure $ CFunction cRet name [] stmts

translateOp :: (MonadError Error m) => TCtx -> Index -> OP -> m [CStmt]
translateOp tctx i op =
  case op of
    PUSHI x  -> pure [CStmt $ "stack[sp++] = " <> toByteString x <> ";"]
    PUSHR i  -> pure [CStmt $ "stack[sp++] = registers[" <> toByteString i <> "];"]
    POPR i   -> pure [CStmt $ "registers[" <> toByteString i <> "] = stack[--sp];"]
    HALT     -> pure [CStmt "return 0;"]
    RET      -> pure [ CStmt "retval = stack[--sp];"
                     , CStmt "DUMMY = stack[--sp];"
                     , CStmt "return;"
                     ]
    RETV     -> pure [ CStmt "DUMMY = stack[--sp];"
                     , CStmt " return;"
                     ]
    SVC      -> pure [ CStmt "service();" ]
    CALL f   -> pure [ CStmt "stack[sp++] = DUMMY;"
                     , CStmt $ f <> "();"
                     , CStmt "stack[sp++] = retval;"
                     ]
    GOTO d   -> do
      label <- lookupLabel tctx (i + d)
      pure [ CStmt $ "goto " <> label]
    BZ d     -> do
      label <- lookupLabel tctx (i + d)
      pure [ CStmt "e1 = stack[--sp];"
           , CStmt $ "if (!e1) goto " <> label <> ";"
           ]
    ADD      -> pure $ cBinOp "+"
    SUB      -> pure $ cBinOp "-"
    MUL      -> pure $ cBinOp "*"
    DIV      -> pure $ cBinOp "/"
    AND      -> pure $ cBinOp "&&"
    OR       -> pure $ cBinOp "||"
    EQ       -> pure $ cBinOp "=="
    NEQ      -> pure $ cBinOp "!="
    GT       -> pure $ cBinOp ">"
    LT       -> pure $ cBinOp "<"
    LTEQ     -> pure $ cBinOp "<="
    GTEQ     -> pure $ cBinOp ">="
    NEG      -> pure $ cUnOp "-"
    NOT      -> pure $ cUnOp "!"
    RW       -> undefined -- TODO
    RB       -> undefined -- TODO
    WW       -> undefined -- TODO
    WB       -> undefined -- TODO

lookupLabel :: (MonadError Error m) => TCtx -> Index -> m Ident
lookupLabel tctx i =
  case tctx V.!? fromIntegral i of
    Nothing -> throwError $ "Error looking up label for index " <> fromString (show i)
    Just label -> pure label

cBinOp op = [ CStmt "e1 = stack[--sp];"
            , CStmt "e2 = stack[--sp];"
            , CStmt $ "stack[sp++] = e1 " <> op <> " e2;"
            ]

cUnOp op  = [ CStmt "e1 = stack[--sp];"
            , CStmt $ "stack[sp++] = " <> op <> "e1;"
            ]

serviceFuncText = B.unlines
  [ " void service() {"
  , "   int code = stack[--sp];"
  , " "
  , "   int tmp_int;"
  , "   unsigned char tmp_byte;"
  , "   char tmp_char;"
  , "   unsigned int addr;"
  , " "
  , " "
  , "   switch (code) {"
  , "     case 1: // print int"
  , "       tmp_int = stack[--sp];"
  , "       printf(\"%d\", tmp_int);"
  , "       break;"
  , "     case 2: // print byte"
  , "       tmp_byte = stack[--sp];"
  , "       printf(\"%d\", tmp_byte);"
  , "       break;"
  , "     case 3: // print char"
  , "       tmp_char = stack[--sp];"
  , "       printf(\"%c\", tmp_char);"
  , "       break;"
  , "     case 4: // print str TODO"
  , "       break;"
  , "     case 11: // read int from memory"
  , "       addr = stack[--sp];"
  , "       stack[sp++] = ((int *) mem)[addr >> 2];"
  , "       break;"
  , "     case 12: // read byte from memory"
  , "       addr = stack[--sp];"
  , "       stack[sp++] = mem[addr];"
  , "     case 14: // write word int from memory TODO"
  , "       addr = stack[--sp];"
  , "       ((int *) mem)[addr >> 2] = stack[--sp];"
  , "       break;"
  , "     default:"
  , "       break;"
  , "   }"
  , " }"
  ]

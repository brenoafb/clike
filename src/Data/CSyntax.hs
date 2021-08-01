{-# LANGUAGE OverloadedStrings #-}

module Data.CSyntax where

import Typedefs
import Utils
import qualified Data.ByteString as B
import Data.List (intercalate)

data CProgram = CProgram
  { cIncludes  :: [CInclude]
  , cDefines   :: [CDefine]
  , cGlobals   :: [CDecl]
  , cText      :: B.ByteString
  , cFunctions :: [CFunction]
  }

instance Show CProgram where
  show (CProgram imports defines globals text funcs) =
       importStr <> "\n\n"
    <> defineStr <> "\n\n"
    <> globalStr <> "\n\n"
    <> textStr   <> "\n\n"
    <> funcStr
      where
        importStr = intercalate "\n" $ map show imports
        defineStr = intercalate "\n" $ map show defines
        globalStr = intercalate "\n" $ map show globals
        textStr   = bs2str text
        funcStr   = intercalate "\n" $ map show funcs

data CInclude = CLibInclude Ident
              | CLocalInclude Ident
  deriving Eq

instance Show CInclude where
  show (CLibInclude id)   = "#include <"  <> bs2str id <> ">"
  show (CLocalInclude id) = "#include \"" <> bs2str id <> "\""

data CDefine = CDefine Ident Ident
  deriving Eq

instance Show CDefine where
  show (CDefine name c) = "#define " <> bs2str name <> " " <> bs2str c

data CFunction = CFunction CType Ident [CDecl] [CStmt]

instance Show CFunction where
  show (CFunction rettype name args body) =
    show rettype <> " " <> bs2str name
    <> "(" <> argsStr <> ")"
    <> "\n{\n" <> bodyStr <> "\n}\n"
      where argsStr = intercalate ", " $ map show args
            bodyStr = intercalate "\n" $ map show body

data CDecl = CDecl CType Ident
           | CInit CType Ident Ident
           | CArrDecl CType Ident Ident
  deriving Eq

instance Show CDecl where
  show (CDecl t s)      = show t <> bs2str ( " " <> s <> ";")
  show (CInit t s e)    = show t <> bs2str ( " " <> s <> " = " <> e <> ";")
  show (CArrDecl t s n) = show t <> bs2str ( " " <> s <> "["   <> n <> "];")

data CType = CInt
           | CChar
           | CUChar
           | CVoid
           deriving Eq

instance Show CType where
  show CInt = "int"
  show CChar = "char"
  show CUChar = "unsigned char"
  show CVoid = "void"

newtype CStmt = CStmt Ident
  deriving Eq

instance Show CStmt where
  show (CStmt stmt) = bs2str stmt

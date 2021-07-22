{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bytecode
import Parser
import Typechecker
import Compiler
import VM

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import System.Environment
import qualified Data.ByteString as B


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    [filename] -> executeProgram filename
    ["-t", filename] -> typecheckProgram filename
    ["-p", filename] -> printAST filename
    ["-c", filename] -> compileProgram filename
    ["-e", filename] -> executeProgram filename
    ["-s", filename] -> buildSymbolTables filename

compileProgram :: FilePath -> IO ()
compileProgram file = do
  code <- readFile file
  let program = parseStr code
  case compile program of
    Left err -> B.putStrLn $ "Compiler error: " <> err
    Right bc -> print bc

nRegsCONST = 32
memSizeCONST = 1024

executeProgram :: FilePath -> IO ()
executeProgram file = do
  code <- readFile file
  let program = parseStr code
  case compile program of
    Left err -> B.putStrLn $ "Compiler error: " <> err
    Right bc@(Bytecode constants functions) -> do
      let vm = initVM nRegsCONST memSizeCONST constants
          ft = mkFunctionTable functions
       in do
         result <- evalStateT (runReaderT (runExceptT executeVM) ft) vm
         case result of
           Left err -> print err
           Right () -> print "Done!"

typecheckProgram :: FilePath -> IO ()
typecheckProgram file = do
  code <- readFile file
  let program = parseStr code
      typechecks = typecheck program
  case typecheck program of
    Just err -> B.putStrLn $ "Type check error: " <> err
    Nothing -> return ()

printAST :: FilePath -> IO ()
printAST file = do
  code <- readFile file
  let ast = parseStr code
  print ast

buildSymbolTables = undefined

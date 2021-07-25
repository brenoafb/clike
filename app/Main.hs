{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bytecode
import Data.Syntax
import Parser.Parser
import Compiler.Typechecker
import Compiler.Compiler
import VM.VM

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import System.Environment
import qualified Data.ByteString as B
import qualified Data.Binary as Bin

nRegsCONST = 16
memSizeCONST = 1024

main :: IO ()
main = do
  -- currentDirectory <- getCurrentDirectory
  -- contents <- getDirectoryContents currentDirectory
  -- print currentDirectory
  -- print contents
  args <- getArgs
  case args of
    [] -> return ()
    [filename] -> executeProgram filename
    ["-t", filename]             -> typecheckProgram filename
    ["-p", filename]             -> printAST filename
    ["-c", filename]             -> compileProgram filename "bc"
    ["-c", filename, bcFilename] -> compileProgram filename bcFilename
    ["-e", filename]             -> executeProgram filename
    ["-s", filename]             -> buildSymbolTables filename

compileProgram :: FilePath -> FilePath -> IO ()
compileProgram file out = do
  code <- readFile file
  let program@(Program imports functions) = parseStr code
  result <- runExceptT $ compile program
  case result of
    Left err -> B.putStrLn $ "Compiler error: " <> err
    Right bc -> do
      let importNames = map (\(Import dep) -> dep) imports
      libs <- liftIO $ mapM loadBytecode importNames
      let linkedBC = linkBytecode $ bc : libs
      Bin.encodeFile out linkedBC
      putStrLn "Bytecode written do 'bc'"

executeProgram :: FilePath -> IO ()
executeProgram file = do
  bc <- Bin.decodeFile file
  let (Bytecode constants functions) = bc
      vm = initVM nRegsCONST memSizeCONST constants
      ft = mkFunctionTable $ map (\(n, _, c) -> (n, c)) functions
  result <- runExceptT (execStateT (runReaderT executeVM ft) vm)
  case result of
    Left err -> B.putStrLn err
    Right vm -> pure ()

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

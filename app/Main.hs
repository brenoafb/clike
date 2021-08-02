{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bytecode
import Data.Syntax
import Data.CSyntax
import Parser.Parser
import Compiler.Typechecker
import Compiler.Compiler
import Translator.Translator
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
    ["-pb", filename]            -> printBytecode filename
    ["-c", filename]             -> compileProgram filename "bc"
    ["-c", filename, bcFilename] -> compileProgram filename bcFilename
    ["-e", filename]             -> executeProgram filename
    ["-tr", filename]             -> translate filename
    ["-cc", filename]             -> compileC filename

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
      putStrLn $ "Bytecode written to " <> out

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

printBytecode :: FilePath -> IO ()
printBytecode file  = do
  code <- readFile file
  let program@(Program imports functions) = parseStr code
  result <- runExceptT $ compile program
  case result of
    Left err -> B.putStrLn $ "Compiler error: " <> err
    Right bc -> do
      let importNames = map (\(Import dep) -> dep) imports
      libs <- liftIO $ mapM loadBytecode importNames
      let linkedBC = linkBytecode $ bc : libs
      print linkedBC

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

translate :: FilePath -> IO ()
translate file = do
  bc <- Bin.decodeFile file
  case runExcept $ translateBytecode bc of
    Left err -> print err
    Right cprog -> print cprog

compileC :: FilePath -> IO ()
compileC file = do
  code <- readFile file
  let program@(Program imports functions) = parseStr code
  result <- runExceptT $ compile program
  case result of
    Left err -> B.putStrLn $ "Compiler error: " <> err
    Right bc -> do
      let importNames = map (\(Import dep) -> dep) imports
      libs <- liftIO $ mapM loadBytecode importNames
      let linkedBC = linkBytecode $ bc : libs
      case runExcept $ translateBytecode bc of
        Left err -> print err
        Right cprog -> print cprog

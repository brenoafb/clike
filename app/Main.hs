{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Typechecker
import System.Environment
import Compiler
import qualified Data.ByteString as B

import ConstantTable

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    [filename] -> executeProgram filename
    ["-t", filename] -> typecheckProgram filename
    ["-p", filename] -> printAST filename
    ["-c", filename] -> compileProgram filename
    ["-s", filename] -> buildSymbolTables filename

compileProgram :: FilePath -> IO ()
compileProgram file = do
  code <- readFile file
  let program = parseStr code
  case compile program of
    Left err -> B.putStrLn $ "Compiler error: " <> err
    Right bc -> print bc


executeProgram :: FilePath -> IO ()
executeProgram file = undefined

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

buildConstantsTable :: FilePath -> IO ()
buildConstantsTable file = do
  code <- readFile file
  let program = parseStr code
      ct = mkConstantTable program
  print ct

buildSymbolTables = undefined

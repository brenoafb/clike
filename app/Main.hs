{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Typechecker
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
